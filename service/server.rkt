#lang racket/base

(require web-server/servlet-env
         web-server/servlet

         "cds-services.rkt")

;;; ================================= CORS =====================================

(define cors-headers
  (list (make-header #"Access-Control-Allow-Origin"
                     #"https://sandbox.cds-hooks.org")
        (make-header #"Access-Control-Allow-Methods"
                     #"GET, POST, OPTIONS")
        (make-header #"Access-Control-Allow-Headers"
                     #"Content-Type, Authorization")))

(define (add-cors resp)
  (struct-copy response resp
               [headers (append cors-headers (response-headers resp))]))

;;; ===================== Top level handlers & routing =========================

(define (not-found req)
  (response/full
   404 #"Not Found"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list)
   (list #"Not Found")))

(define (options req)
  (response/full
   204 #"No Content"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list)
   (list)))

(define-values (cds-dispatch cds-url)
  (dispatch-rules
   [("cds-services") #:method "get" handle-cds-discovery]
   [("cds-services" (symbol-arg)) #:method "post" handle-cds-hook]
   [else not-found]))

(define (app req)
  (add-cors
   (if (bytes=? (request-method req) #"OPTIONS")
       (options req)
       (cds-dispatch req))))

;;; ================================ Serve =====================================

(module+ main
  (displayln "Server started!")
  (serve/servlet app
                 #:port 3000
                 #:servlet-regexp #rx""
                 #:command-line? #t
                 #:log-file (current-output-port))
  (displayln "Server stopped!"))
