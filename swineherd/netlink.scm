; SPDX-FileCopyrightText: 2023 Ricardo Wurmus <rekado@elephly.net>
;
; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (swineherd netlink)
  #:use-module ((guix build syscalls)
                #:select (mount MS_BIND MS_REC MS_SHARED))
  #:use-module (ip link)
  #:use-module (ip utils)
  #:use-module (netlink connection)
  #:use-module (netlink constant)
  #:use-module (netlink deserialize)
  #:use-module (netlink message)
  #:use-module (netlink route attrs)
  #:use-module (netlink route link)
  #:use-module (srfi srfi-1)
  #:use-module (swineherd config)
  #:export (link-set-master
            attach-netns))

;; This is not provided by Guile-Netlink, so we implement it here.
(define (link-set-master device master)
  "Set the bridge master of DEVICE to the bridge MASTER."
  (define request-num (random 65535))
  (define id (if (number? device) device (link-name->index device)))
  (define master-id (if (number? master) master (link-name->index master)))
  (define message
    (make-message
     RTM_NEWLINK
     (logior NLM_F_REQUEST NLM_F_ACK)
     request-num
     0
     (make-link-message
      AF_UNSPEC
      0 id 0 0
      (list (make-route-attr IFLA_MASTER
              (make-u32-route-attr master-id))))))
  (let ((sock (connect-route)))
    (send-msg message sock)
    (let ((answer (receive-and-decode-msg sock %default-route-decoder)))
      (close-port sock)
      (answer-ok? (last answer)))))

(define (attach-netns netns pid)
  (let* ((netns-directory (%config 'netns-run-directory))
         (stat (stat netns-directory))
         (port (open netns-directory
                     (logior O_RDONLY O_DIRECTORY))))
    (dynamic-wind
      (const #true)
      (lambda ()
        (flock port LOCK_EX)

        ;; mount --make-shared
        (catch #true
          (lambda ()
            (mount "" netns-directory
                   "none" (logior MS_SHARED MS_REC)))
          (lambda _
            ;; Upgrade directory to a mount point
            (mount netns-directory netns-directory
                   "none" (logior MS_BIND MS_REC)))))
      (lambda ()
        (false-if-exception
         (flock port LOCK_UN))
        (false-if-exception
         (close-port port))))

    (let* ((netns-file (format #false "~a/~a" netns-directory netns))
           (port (open netns-file (logior O_RDONLY O_CREAT O_EXCL))))
      (catch #true
        (lambda ()
          (close-port port)
          ;; Bind the netns to the run directory
          (mount (format #false "/proc/~a/ns/net" pid)
                 netns-file
                 "none" MS_BIND))
        (lambda ()
          (false-if-exception
           (delete-file netns-file)))))))
