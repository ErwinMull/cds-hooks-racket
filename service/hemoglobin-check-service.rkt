#lang racket/base

(provide check-hemoglobin)

(define low 7.5)
(define high 10)

(define (check-hemoglobin observation)
  (define value
    (let* ([tmp (hash-ref observation 'valueQuantity (hasheq))]
           [tmp (hash-ref tmp 'value #f)])
      tmp))
  (define details (format "~a (low: ~a, high: ~a)" (or value "n/a") low high))
  (cond
    [(not value) (list
                    (hasheq 'summary "No Blood Pressure found"
                            'indicator "info"
                            'details details
                            'source (hasheq
                                     'label "My service")))]
    [(< value low) (list
                    (hasheq 'summary "Blood Pressure: LOW"
                            'indicator "warning"
                            'details details
                            'source (hasheq
                                     'label "My service")))]
    [(> value high) (list
                     (hasheq 'summary "Blood Pressure: HIGH"
                             'indicator "warning"
                             'details details
                             'source (hasheq
                                      'label "My service")))]
    [else (list
           (hasheq 'summary "Blood Pressure: NORMAL"
                   'indicator "info"
                   'details details
                   'source (hasheq
                            'label "My service")))]))
