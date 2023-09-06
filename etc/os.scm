; SPDX-FileCopyrightText: 2023 Ricardo Wurmus <rekado@elephly.net>
;
; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (gnu)
             (ice-9 match)
             (srfi srfi-1)
             (guix packages)
             (guix profiles))
(use-service-modules networking shepherd sysctl)
(use-package-modules bash linux)

(define bash (specification->package "bash"))
(define coreutils (specification->package "coreutils"))

(operating-system
  (host-name "template")
  (timezone "Europe/Berlin")
  (locale "en_US.utf8")
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/sdX"))))
  (file-systems (cons (file-system
                        (device (file-system-label "my-root"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))
  (packages (append
             (specifications->packages
              (list "bash" "coreutils"
                    "guile" "guile-netlink"))
             %base-packages))
  (services
   (append (list
            (service dhcp-client-service-type)
            (service special-files-service-type
                     `(("/usr/bin/env" ,(file-append coreutils "/bin/env"))
                       ("/bin/bash" ,(file-append bash "/bin/bash")))))
           (list (service login-service-type)
                 (service virtual-terminal-service-type)
                 (service console-font-service-type
                          `(("tty1" . ,%default-console-font)))
                 (service syslog-service-type)
                 (service agetty-service-type (agetty-configuration
                                               (extra-options '("-L")) ; no carrier detect
                                               (term "vt100")
                                               (tty #f) ; automatic
                                               (shepherd-requirement '(syslogd))))
                 (service mingetty-service-type (mingetty-configuration
                                                 (tty "tty1")))
                 (service static-networking-service-type
                          (list %loopback-static-networking))
                 (service nscd-service-type)
                 (service udev-service-type)
                 (service sysctl-service-type)
                 (service special-files-service-type
                          `(("/bin/sh" ,(file-append bash "/bin/sh"))
                            ("/usr/bin/env" ,(file-append coreutils "/bin/env"))
                            ("/lib64/ld-linux-x86-64.so.2"
                             ,(file-append glibc "/lib/ld-linux-x86-64.so.2"))))))))
