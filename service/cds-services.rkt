#lang racket/base

(require json
         web-server/http

         "../utils/cds-discovery-utils.rkt"

         "patient-greeting-service.rkt"
         "hemoglobin-check-service.rkt")

(provide handle-cds-discovery
         handle-cds-hook)

;;; ======================= Patient greeting service ===========================

(define patient-greeting-service
  (make-cds-service-discovery
   'patient-view
   'patient-greeting
   "Greets the patient"
   #:title "Patient Greeting"
   #:prefetch (hasheq
               'patient "Patient/{{context.patientId}}")))

(define (patient-greeting-handler req)
  (define body-jsexpr
    (bytes->jsexpr (request-post-data/raw req)))
  (define patient
    (let* ([tmp (hash-ref body-jsexpr 'prefetch (hasheq))]
           [tmp (hash-ref tmp 'patient (hasheq))])
      tmp))
  (response/jsexpr
   (hasheq 'cards
           (append (greet-patient patient)))))

;;; =========================== Hemoglobin alarm ===============================

(define hemoglobin-alarm-service
  (make-cds-service-discovery
   'patient-view
   'hemoglobin-alarm
   "Alarms if the hemoglobin concentration lies outside of the reference range"
   #:title "Hemoglobin Alarm"
   #:prefetch (hasheq
               'hemoglobin "Observation?patient=Patient/{{context.patientId}}&code=718-7&_count=1&_sort:desc=date")))

(define (hemoglobin-alarm-handler req)
  (define body-jsexpr
    (bytes->jsexpr (request-post-data/raw req)))
  (define bundle
    (let* ([tmp (hash-ref body-jsexpr 'prefetch (hasheq))]
           [tmp (hash-ref tmp 'hemoglobin (hasheq))])
      tmp))
  (define hemoglobin
    (let ([tmp (hash-ref bundle 'entry '())])
      (if (> (length tmp) 0)
          (hash-ref (car tmp) 'resource (hasheq))
          (hasheq))))
  (response/jsexpr
   (hasheq 'cards
           (append (check-hemoglobin hemoglobin)))))

;;; ======================= CDS discovery & routing ============================

(define service-handler-list
  (list (cons patient-greeting-service patient-greeting-handler)
        (cons hemoglobin-alarm-service hemoglobin-alarm-handler)))

(define services-routing-table
  (foldl (λ (curr ht)
           (let ([hook (cds-service-discovery-hook (car curr))]
                 [id (cds-service-discovery-id (car curr))]
                 [handler (cdr curr)])
             (let ([tmp (hash-ref ht hook (hasheq))])
               (hash-set ht hook (hash-set tmp id handler)))))
         (hasheq)
         service-handler-list))

(define services-discovery-information
  (hasheq 'services
          (map (λ (x)
                 (cds-service-discovery->jsexpr (car x)))
               service-handler-list)))

(define (handle-cds-discovery req)
  (response/jsexpr
   services-discovery-information))

(define (handle-cds-hook req id)
  (define body (request-post-data/raw req))
  (define parsed-body (bytes->jsexpr body))
  (define hook (string->symbol (hash-ref parsed-body 'hook)))
  (define handler
    (for/fold ([v services-routing-table])
              ([k (in-list (list hook id))]
               #:break (not v))
      (hash-ref v k #f)))
  (handler req))
