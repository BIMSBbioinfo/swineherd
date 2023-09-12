;; SPDX-FileCopyrightText: 2023 Ricardo Wurmus <rekado@elephly.net>
;;
;; SPDX-License-Identifier: CC0-1.0

(use-modules (swineherd)
             (shepherd service))
(register-services (list swineherd-service
                         swineherd-api-server-service))
(start-service swineherd-service)
