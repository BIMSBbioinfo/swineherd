(use-modules (swineherd)
             (shepherd service))
(register-services (list swineherd-service
                         swineherd-api-server-service))
(start-service swineherd-service)
