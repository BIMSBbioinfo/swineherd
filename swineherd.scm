; SPDX-FileCopyrightText: 2023 Ricardo Wurmus <rekado@elephly.net>
;
; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (swineherd)
  #:use-module (config)
  #:use-module (gnu build linux-container)
  #:use-module ((guix build syscalls)
                #:select (umount MNT_DETACH))
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ip addr)
  #:use-module (ip link)
  #:use-module (shepherd service)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (swineherd api)
  #:use-module (swineherd config)
  #:use-module (swineherd netlink)
  #:export (guix-system-container
            swineherd-service
            swineherd-api-server-service))

(define (get-pid running)
  (call-with-input-file
      (format #false "/proc/~a/task/~a/children"
              running running)
    read))

(define (get-tasks pid)
  (map string->number
       (scandir (format #false "/proc/~a/task" pid)
                (lambda (f) (not (member f '("." "..")))))))

(define (proctree pid)
  (let* ((tasks (get-tasks pid))
         (children
          (append-map (lambda (task)
                        (call-with-input-file
                            (format #false "/proc/~a/task/~a/children"
                                    pid task)
                          (lambda (port)
                            (let loop ((tokens '())
                                       (token (read port)))
                              (if (eof-object? token)
                                  (reverse tokens)
                                  (loop (cons token tokens)
                                        (read port)))))))
                      tasks)))
    (append tasks children (append-map proctree children))))

(define (procstats pid)
  (let ((statfile (format #false "/proc/~a/stat" pid))
        (fields '(pid comm state
                      ppid pgrp session
                      tty_nr tpgid flags
                      minflt cminflt majflt cmajflt
                      utime stime cutime cstime
                      priority nice num_threads
                      itrealvalue starttime
                      vsize rss rsslim
                      startcode endcode startstack
                      kstkesp kstkeip
                      signal blocked
                      sigignore sigcatch
                      wchan nswap cnswap
                      exit_signal
                      processor rt_priority
                      policy delayacct_blkio_ticks
                      guest_time cguest_time
                      start_data end_data
                      start_brk arg_start arg_end
                      env_start env_end
                      exit_code)))
    (map cons
         fields
         (call-with-input-file statfile
           (lambda (port)
             (let loop ((tokens '())
                        (token (read port)))
               (if (eof-object? token)
                   (reverse tokens)
                   (loop (cons token tokens)
                         (read port)))))))))

(define* (create-subvolume id #:key (root (%config 'container-root)))
  "Create a btrfs subvolume named ID in the btrfs root file system."
  (let ((subvolume (format #false "~a/~a" root id)))
    (or (zero? (system* (%config 'btrfs)
                        "subvolume" "create"
                        subvolume))
        (error "failed to create btrfs subvolume"))
    subvolume))

(define* (destroy-subvolume id #:key (root (%config 'container-root)))
  "Destroy the btrfs subvolume for the container with the given ID."
  (system* (%config 'btrfs)
           "subvolume" "delete"
           (format #false "~a/~a" root id)))


;;; Actions
(define (action:pid running)
  "Show the PID of the system container."
  (let ((pid (get-pid running)))
    (display (match pid
               ((? eof-object?) "")
               (_ pid)))))

(define (action:exec running . command)
  "Run a command in the container.  Return exit code."
  (let ((pid (get-pid running)))
    (container-excursion pid
      (lambda () (apply system* command)))))

(define (action:peek running file pattern)
  "Display one line of the given file in the container."
  (let* ((pid (get-pid running))
         (rx (make-regexp pattern))
         (filename (format #false "/proc/~a/root~a"
                           pid file)))
    (unless (file-exists? filename)
      (error (format #false "File not found: ~a" filename)))

    ;; We query the file size, so that we can stop reading even
    ;; if we don't encounter eof (e.g. because the file is still
    ;; being written to).
    (let* ((st (stat filename))
           (size (stat:size st)))
      (display (call-with-input-file filename
                 ;; Find the last line matching regexp
                 (lambda (port)
                   (let ((last-line ""))
                     (letrec ((get-line
                               (lambda (last-line)
                                 ;; Stop reading once we reach the
                                 ;; place of the last line when we started.
                                 (if (>= (ftell port) size)
                                     last-line
                                     (match (read-line port)
                                       ((? eof-object?) last-line)
                                       (line (if (regexp-exec rx line)
                                                 (get-line line)
                                                 (get-line last-line))))))))
                       (get-line last-line)))))))))

(define (action:ip running)
  "Show the IP address of the system container."
  (let* ((pid (get-pid running))
         (address
          (container-excursion* pid
            (lambda ()
              (catch #true
                (lambda ()
                  (let ((name (format #false "ceth-~a" pid)))
                    (and=>
                     (find (lambda (link)
                             (string=? (link-name link) name))
                           (get-links))
                     (lambda (link)
                       (and=>
                        (find (lambda (addr)
                                (eq? ((@@ (ip addr) addr-link) addr)
                                     (link-id link)))
                              ((@@ (ip addr) get-addrs)))
                        (@@ (ip addr) addr-addr))))))
                (lambda _ ""))))))
    (when address
      (display address))))

(define (action:up running)
  "Connect network for the system container."
  (let* ((pid (get-pid running))
         (ns (format #false "guix-~a" pid))
         (host (format #false "veth-~a" pid))
         (client (format #false "ceth-~a" pid)))
    ;; Make existing network namespace available to ip netns
    (attach-netns ns pid)

    ;; Create veth pair and move the client side into the container.
    (link-add host "veth"
              #:type-args `((peer . ,client)))
    (link-set host #:up #true)
    (link-set client #:netns ns)

    ;; Attach host side to host bridge
    (link-set-master host "swineherd0")

    ;; Bring up interface in container
    (container-excursion pid
      (lambda ()
        (link-set "lo" #:up #true)
        (link-set client #:up #true)))))

(define (action:down running)
  "Disconnect network for the system container."
  (let* ((pid (get-pid running))
         (ns (format #false "guix-~a" pid))
         (host (format #false "veth-~a" pid))
         (nsfile (format #false "~a/~a"
                         (%config 'netns-run-directory) ns)))
    (umount nsfile MNT_DETACH)
    (delete-file nsfile)
    (link-del host)))

(define (action:stats running)
  "Print cumulative container usage statistics: bytes received,
bytes transmitted, and CPU time."
  (and-let* ((pid (get-pid running))
             (template (lambda (what)
                         (format #false "/sys/class/net/veth-~a/statistics/~a_bytes" pid what)))
             (rx (call-with-input-file (template "rx") read))
             (tx (call-with-input-file (template "tx") read))
             (cputime (fold (lambda (pid acc)
                              (let ((stats (procstats pid)))
                                (+ acc
                                   (assoc-ref stats 'utime)
                                   (assoc-ref stats 'stime)
                                   (assoc-ref stats 'cutime)
                                   (assoc-ref stats 'cstime))))
                            0 (proctree pid))))
    (format #true "receive:~a transmit:~a cputime:~a"
            rx tx cputime)))

(define (guix-system-container id script script-args)
  (service
   `(,(string->symbol (string-append "swine:" id)))
   #:documentation "Run a Guix System container"
   #:start
   (lambda ()
     ;; Create btrfs subvolume
     (let ((subvolume (create-subvolume id)))
       (chdir subvolume)
       ;; TODO: try to do without /bin/sh -c.
       (fork+exec-command
        (list "/bin/sh" "-c"
              (string-join (append
                            (list "exec" script
                                  (format #false "--share=~a=/home" subvolume))
                            script-args) " ")))))
   #:stop
   (lambda (running . rest)
     (false-if-exception (action:down running))
     (destroy-subvolume id)
     (apply (make-kill-destructor) running rest))
   #:actions
   (actions
    (pid   action:pid)
    (exec  action:exec)
    (peek  action:peek)
    (ip    action:ip)
    (up    action:up)
    (down  action:down)
    (stats action:stats))))


(define (swineherd:up . any)
  (let ((bridge-exists?
         (find (lambda (link)
                 (string=? (link-name link) "swineherd0"))
               (get-links))))
    (unless bridge-exists?
      (link-add "swineherd0" "bridge")
      (link-set "swineherd0" #:up #true)
      (addr-add "swineherd0" "10.0.0.1/16"))))
(define (swineherd:down . any)
  (link-del "swineherd0"))

(define swineherd-service
  (service
   '(swineherd)
   #:documentation "Run the Guix System container manager"
   #:one-shot? #true
   #:start
   (lambda args
     ;; Initialize %config
     (let ((opts (getopt-config-auto (cons "swineherd" args) config)))
       (%config opts))
     (swineherd:up)
     #true)
   #:actions
   (actions
    (up swineherd:up)
    (down swineherd:down)
    (new
     (lambda (running id script . args)
       (system* (%config 'herd)
                "-s" (%config 'socket-file)
                "eval" "root"
                (object->string
                 `(begin
                    (use-modules (swineherd))
                    (let ((service (guix-system-container
                                    ,id
                                    ,script
                                    ',args)))
                      (register-services (list service))
                      (start-service service)))))))
    (kill
     (lambda (running id)
       (system* (%config 'herd)
                "-s" (%config 'socket-file)
                "unload" "root"
                (format #false "swine:~a" id)))))))

(define swineherd-api-server-service
  (service
   '(swineherd-http-api)
   #:documentation "Run the Swineherd's HTTP API server"
   #:start
   (lambda args
     ;; Initialize %config
     (let ((opts (getopt-config-auto
                  (cons* "swineherd" "api-server" args) config)))
       (%config opts))
     (fork+exec-command
      (list (%config 'guile) "-c"
            (object->string
             `(begin
                (use-modules (swineherd api))
                (run-swineherd-api-server ,(%config 'host)
                                          ,(%config 'port)))))))
   #:stop
   (make-kill-destructor)))

;; Local Variables:
;; eval: (put 'container-excursion* 'scheme-indent-function 1)
;; End:
