#lang racket/base

(provide check-hemoglobin)

(define (extract-low&high reference-ranges)
  (cond
    [(null? reference-ranges) (values #f #f)]
    [else (let* ([first-range (car reference-ranges)]
                 [low-quantity (hash-ref first-range 'low (hasheq))]
                 [low (hash-ref low-quantity 'value #f)]
                 [high-quantity (hash-ref first-range 'high (hasheq))]
                 [high (hash-ref high-quantity 'value #f)])
            (values low high))]))

(define (check-hemoglobin observation)
  (define value
    (let* ([tmp (hash-ref observation 'valueQuantity (hasheq))]
           [tmp (hash-ref tmp 'value #f)])
      tmp))
  (define-values (low high)
    (extract-low&high (hash-ref observation 'referenceRange '())))
  (define details (format "~a (low: ~a, high: ~a)" value low high))
  (cond
    [(not value) (list
                  (hasheq 'summary "No Blood Pressure found"
                          'indicator "info"
                          'source (hasheq
                                   'label "My service")))]
    [(and (not low) (not high)) (list
                                 (hasheq 'summary "No Reference Range given"
                                         'indicator "info"
                                         'source (hasheq
                                                  'label "My service")))]
    [(and low (< value low)) (list
                              (hasheq 'summary "Blood Pressure: LOW"
                                      'indicator "warning"
                                      'detail details
                                      'source (hasheq
                                               'label "My service")))]
    [(and high (> value high)) (list
                                (hasheq 'summary "Blood Pressure: HIGH"
                                        'indicator "warning"
                                        'detail details
                                        'source (hasheq
                                                 'label "My service")))]
    [else (list
           (hasheq 'summary "Blood Pressure: NORMAL"
                   'indicator "info"
                   'detail details
                   'source (hasheq
                            'label "My service")))]))
