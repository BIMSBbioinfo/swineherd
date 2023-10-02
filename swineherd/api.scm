; SPDX-FileCopyrightText: 2023 Ricardo Wurmus <rekado@elephly.net>
;
; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (swineherd api)
  #:use-module (swineherd config)
  #:use-module (swineherd utils)
  #:use-module ((ice-9 iconv) #:select (bytevector->string))  
  #:use-module (ice-9 match)
  #:use-module (fibers web server)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:use-module (system repl error-handling)
  #:export (run-swineherd-api-server))


(define-condition-type &herd-unknown-service-error &error
  herd-unknown-service-error?
  (service herd-unknown-service-service))

(define-condition-type &api-missing-field-error &error
  api-missing-field-error?
  (irritant api-missing-field-irritant))

(define-condition-type &api-request-error &error
  api-request-error?
  (message api-request-error-message))

(define-condition-type &api-internal-error &error
  api-internal-error?
  (message api-internal-error-message))

(define* (read-payload bv #:optional (mandatory '()))
  (let* ((parsed (json-string->scm
                  (bytevector->string bv "utf-8")))
         (get (lambda (key)
                (or (assoc-ref parsed key)
                    (raise (condition
                            (&api-missing-field-error
                             (irritant key))))))))
    (for-each get mandatory)
    get))

(define* (render-json json #:optional (headers '()))
  (list (append '((content-type . (application/json))) headers)
        (lambda (port)
          (scm->json json port))))

(define (not-found uri)
  (list (build-response #:code 404)
        (string-append "Resource not found: " (uri->string uri))))


(define (herd service action . args)
  "Talk to the socket of the Shepherd instance that supervises our
containers.  This assumes that the API server has read+write
permissions on the shepherd socket."
  (let* ((command
          ;; TODO: allow user to pass both prefix commands (such as
          ;; "sudo") and extra arguments to herd.
          (cons* (%config 'herd)
                 "-s" (%config 'socket-file) "--"
                 action service args))
         (lines (list))
         (proc (lambda (line)
                 (when (string=? (format #false "herd: service '~a' could not be found~%"
                                         service)
                                 line)
                   ;; Don't raise condition here, because it would be
                   ;; handled by CALL-WITH-OUTPUT-PROCESSOR's catch
                   ;; first and rethrown as a compound condition.
                   (throw 'unknown-service))
                 (set! lines (cons line lines)))))
    (catch 'unknown-service
      (lambda ()
        (let ((ret (call-with-output-processor command proc)))
          (values ret lines)))
      (lambda _
        (raise (condition
                (&herd-unknown-service-error
                 (service service))))))))

(define (vm-id->service-name vm-id)
  (string-append "swine:" vm-id))

(define (destroy vm-id)
  (zero? (herd "swineherd" "kill" vm-id)))

(define (pid vm-id)
  (call-with-values
      (lambda () (herd (vm-id->service-name vm-id) "pid"))
    (lambda (exit-code lines)
      ;; There could be an error message, or there could be no output
      ;; at all.
      (match lines
        (((? string? output) . rest)
         (false-if-exception
          (string->number (string-trim-right output))))
        (_ #false)))))

(define (up vm-id)
  (zero? (herd (vm-id->service-name vm-id) "up")))

(define (down vm-id)
  (zero? (herd (vm-id->service-name vm-id) "down")))

(define (exec vm-id command)
  (zero? (apply herd (vm-id->service-name vm-id) "exec" command)))

(define (peek-file vm-id file pattern)
  (call-with-values
      (lambda () (herd (vm-id->service-name vm-id) "peek" file pattern))
    (lambda (exit-code lines)
      ;; There could be an error message, or there could be no output
      ;; at all.
      (and (zero? exit-code)
           (match lines
             (((? string? output) . rest) output)
             (_ #false))))))

(define (valid-ip? ip)
  (or (and (false-if-exception (inet-pton AF_INET ip)) 4)
      (and (false-if-exception (inet-pton AF_INET6 ip)) 6)))

(define (ips vm-id)
  "Return a list of all IP addresses and the CIDR for the container
with the given VM-ID."
  (call-with-values
      (lambda () (herd (vm-id->service-name vm-id) "ip"))
    (lambda (exit-code lines)
      ;; There could be an error message, or there could be no output at
      ;; all, so we only pass the output on if it looks like an IP.
      (filter-map
       (lambda (line)
         (match (string-split (string-trim-right line) #\/)
           ((addr)
            (and=> (valid-ip? addr)
                   (lambda (type)
                     `((addr . ,addr)
                       (cidr . ,#false)
                       (type . ,type)))))
           ((addr prefix)
            (and=> (valid-ip? addr)
                   (lambda (type)
                     `((addr . ,addr)
                       (cidr . ,prefix)
                       (type . ,type)))))
           (_ #false)))
       (filter string? lines)))))

(define (stats vm-id)
  (call-with-values
      (lambda () (herd (vm-id->service-name vm-id) "stats"))
    (lambda (exit-code lines)
      ;; There could be an error message, or there could be no output
      ;; at all, so we only pass the output on if it matches a
      ;; specific format.
      (match lines
        (((? string? output) . rest)
         (match (string-tokenize output
                                 (char-set-complement
                                  (char-set #\space #\newline #\:)))
           (("receive" receive "transmit" transmit "cputime" cputime)
            (values (string->number receive)
                    (string->number transmit)
                    (string->number cputime)))
           (_ (values #false #false #false))))
        (_ (values #false #false #false))))))

(define (list-containers)
  "Return a list of container names.  The names are those reported by
Shepherd."
  (let ((prefix " + swine:"))
    (call-with-values
        (lambda () (herd "root" "status"))
      (lambda (exit-code lines)
        (filter-map (lambda (line)
                      (and (string-prefix? prefix line)
                           (string-drop line (string-length prefix))))
                    lines)))))

(define (launch vm-id script args)
  "Launch the SCRIPT for the Guix System container with identifier
VM-ID as a Shepherd service.  This registers a new unique service for
this VM-ID and makes service actions available to view the PID of the
container process, bring the virtual network up or down, or report
information from inside the container."
  (and (string-prefix? "/gnu/store" script)
       (or (string-suffix? "-run-container" script)
           (string-suffix? "-run-vm" script))
       (zero? (apply herd "swineherd" "new" vm-id script args))))

;; Commands that may be triggered via API request at
;; /api/container/<id>/exec.
(define %permitted-commands
  (list))

(define (permitted-command? command)
  (or (and=> (%config 'permitted-command-prefix)
         (lambda (prefix) (string-prefix? prefix command)))
      (member command %permitted-commands)))

(define (controller request body)
  (guard (c ((herd-unknown-service-error? c)
             (render-json `((error . "request error")
                            (message . ,(format #false "unknown container: `~a'"
                                                (herd-unknown-service-service c))))))
            ((api-missing-field-error? c)
             (render-json `((error . "missing JSON field")
                            (message . ,(api-missing-field-irritant c)))))
            ((api-request-error? c)
             (render-json `((error . "request error")
                            (message . ,(api-request-error-message c)))))
            ((api-internal-error? c)
             (render-json `((error . "internal error")
                            (message . ,(api-internal-error-message c))))))
    (match (cons (request-method request)
                 (request-path-components request))
      (('POST "api" "container" id "launch")
       (let* ((get
               (read-payload body '("prefix"
                                    "directory-map"
                                    "script"
                                    "derivations")))
              (prefix        (get "prefix"))
              (directory-map (get "directory-map"))
              (script        (get "script"))
              ;; Let Guix download/copy all derivations and their
              ;; outputs.
              (outputs
               (match (vector->list (get "derivations"))
                 (() #true)
                 (derivations
                  (zero? (apply system* (%config 'guix)
                                "build" "--fallback"
                                derivations)))))
              (success?
               (and outputs
                    (launch (string-append prefix ":" id)
                            script
                            (filter-map
                             (lambda (mapping)
                               (and (file-exists? (assoc-ref mapping "source"))
                                    (let ((verb (if (assoc-ref mapping "read-only")
                                                    "expose" "share")))
                                      (format #false "--~a=~a=~a"
                                              verb
                                              (assoc-ref mapping "source")
                                              (assoc-ref mapping "target")))))
                             (vector->list directory-map))))))
         (render-json `((success . ,success?)))))
      (('POST "api" "container" id "exec")
       (let* ((get       (read-payload body '("prefix" "command" "arguments")))
              (prefix    (get "prefix"))
              (command   (get "command")))
         ;; This is potentially dangerous, so we restrict commands to
         ;; a carefully curated list of commands.
         (if (permitted-command? command)
             (let ((success? (exec (string-append prefix ":" id)
                                   (cons command
                                         (vector->list (get "arguments"))))))
               (render-json `((success . ,success?))))
             (raise (condition
                     (&api-request-error
                      (message "command not permitted")))))))
      (('PUT "api" "container" id "connect")
       (let* ((get      (read-payload body '("prefix")))
              (prefix   (get "prefix"))
              (success? (up (string-append prefix ":" id))))
         (render-json `((success . ,success?)))))
      (('PUT "api" "container" id "disconnect")
       (let* ((get      (read-payload body '("prefix")))
              (prefix   (get "prefix"))
              (success? (down (string-append prefix ":" id))))
         (render-json `((success . ,success?)))))
      (('DELETE "api" "container" id "destroy")
       (let* ((get (read-payload body '("prefix")))
              (container-id (string-append (get "prefix") ":" id)))
         (or (begin
               (down container-id)      ;ignore failure
               (destroy container-id))
             (raise (condition
                     (&api-internal-error
                      (message "could not destroy container")))))
         (render-json `((success . ,#true)
                        (message . ,(format #false
                                            "destroyed container ~a"
                                            id))))))
      (('GET "api" "containers")
       (render-json
        `((containers . ,(list->vector (list-containers))))))
      (('GET "api" "container" id "pid")
       (let* ((get (read-payload body '("prefix")))
              (container-id (string-append (get "prefix") ":" id))
              (pid (retry (lambda _ (pid container-id))
                          #:retries 10
                          #:wait (lambda (n) (sleep (1+ 1))))))
         (unless pid
           (raise (condition
                   (&api-internal-error
                    (message "failed to gather information due to timeout")))))
         (render-json `((success . ,#true)
                        (pid . ,pid)))))
      (('GET "api" "container" id "info")
       (let* ((get (read-payload body '("prefix")))
              (container-id (string-append (get "prefix") ":" id))
              (pid (retry (lambda _ (pid container-id))
                          #:retries 10
                          #:wait (lambda (n) (sleep (1+ 1)))))
              (received transmitted cputime (stats container-id)))
         (unless pid
           (raise (condition
                   (&api-internal-error
                    (message "failed to gather information due to timeout")))))
         (render-json `((success . ,#true)
                        (id . ,id)
                        (container . ,container-id)
                        (ip . ,(list->vector (ips container-id)))
                        (received . ,received)
                        (transmitted . ,transmitted)
                        (cputime . ,cputime)
                        (pid . ,pid)))))
      (('GET "api" "container" id "peek")
       (let* ((get (read-payload body '("prefix" "file" "pattern")))
              (container-id (string-append (get "prefix") ":" id)))
         (render-json `((success . ,#true)
                        (result . ,(peek-file container-id (get "file") (get "pattern")))))))
      (_ (not-found (request-uri request))))))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (handler request body)
  (format (current-error-port)
          "~a ~a~%"
          (request-method request)
          (uri-path (request-uri request)))
  (if (getenv "SWINEHERD_DEBUG")
      (call-with-error-handling
        (lambda ()
          (apply values (controller request body))))
      (apply values (controller request body))))

;;; A common buffer size value used for the TCP socket SO_SNDBUF option and
;;; the gzip compressor buffer size.
(define %default-buffer-size
  (* 208 1024))

(define %default-socket-options
  ;; List of options passed to 'setsockopt' when transmitting files.
  (list (list SO_SNDBUF %default-buffer-size)))

(define* (configure-socket socket #:key (level SOL_SOCKET)
                           (options %default-socket-options))
  "Apply multiple option tuples in OPTIONS to SOCKET, using LEVEL."
  (for-each (cut apply setsockopt socket level <>)
            options))

(define (open-server-socket address)
  "Return a TCP socket bound to ADDRESS, a socket address."
  (let ((sock (socket (sockaddr:fam address) SOCK_STREAM 0)))
    (configure-socket sock #:options (cons (list SO_REUSEADDR 1)
                                           %default-socket-options))
    (bind sock address)
    sock))

(define (run-swineherd-api-server host port)
  (let* ((addr (match (getaddrinfo host)
                 ((info _ ...)
                  (addrinfo:addr info))
                 (()
                  (error "failed to look up host"))))
         (address (make-socket-address (sockaddr:fam addr)
                                       (sockaddr:addr addr)
                                       port))
         (socket  (open-server-socket address)))
    (format (current-error-port)
            "Envy API server is running at http://~a:~a~%"
            (inet-ntop (sockaddr:fam address) (sockaddr:addr address))
            (sockaddr:port address))
    (format (current-error-port)
            "Press C-c C-c to quit.~%")
    (run-server handler #:socket socket)))
