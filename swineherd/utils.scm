; SPDX-FileCopyrightText: 2023 Ricardo Wurmus <rekado@elephly.net>
;
; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (swineherd utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:export (call-with-output-processor
            retry))

(define* (call-with-output-processor command proc #:optional capture-stderr?)
  "Silently execute COMMAND, a list of strings representing an
executable with its arguments, and apply PROC to every line printed to
standard output and, optionally when CAPTURE-STDERR? is #T, standard
error.  Return the exit status of COMMAND."
  ;; We can only capture a program's standard error by parameterizing
  ;; current-error-port to a *file* port before using system* or
  ;; open-pipe*.  The process will write its standard error stream to
  ;; the provided file descriptor.  Meanwhile we read from the file
  ;; descriptor (blocking) for new lines until the process exits.
  (match (socketpair PF_UNIX SOCK_STREAM 0)
    ((in . out)
     (let ((err (if capture-stderr?
                    (dup out)
                    (%make-void-port "w"))))
       (catch #true
         (lambda ()
           (let ((thread
                  (parameterize ((current-error-port err)
                                 (current-output-port out))
                    (call-with-new-thread
                     (lambda ()
                       (let ((status
                              (status:exit-val
                               (apply system* command))))
                         (close-port err)
                         (close-port out)
                         status))))))
             (let loop ()
               (match (read-line in 'concat)
                 ((? eof-object?)
                  (for-each
                   (lambda (port)
                     (false-if-exception (close-port port)))
                   (list err out in))
                  (join-thread thread))
                 (line
                  (proc line)
                  (loop))))))
         (lambda (key . args)
           (for-each
            (lambda (port)
              (false-if-exception (close-port port)))
            (list err out in))
           (apply throw key args)))))))

(define* (retry proc #:key (retries 1) (wait (lambda _ (sleep 1))))
  "Run PROC.  If the return value is #F run it again for up to
ATTEMPTS times.  Evaluate WAIT between attempts, a procedure that
takes the number of remaining attempts."
  (let ((value (proc retries)))
    (or value
        (begin
          (wait retries)
          (if (positive? retries)
              (retry proc #:retries (1- retries) #:wait wait)
              #false)))))
