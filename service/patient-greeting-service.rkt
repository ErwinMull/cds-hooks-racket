#lang racket/base

(require racket/string)

(provide greet-patient)

(define (make-patient-greeting-card name)
  (define name-str
    (or (hash-ref name 'text #f)
        (string-join (append (hash-ref name 'given '())
                             (hash-ref name 'family '()))
                     " ")))
  (hasheq 'summary (string-append "Hello, " name-str)
          'indicator "info"
          'source (hasheq
                   'label "My service")))

(define (greet-patient patient)
  (define names (hash-ref patient 'name (hasheq)))
  (define official-names
    (filter (λ (name)
              (string=? (hash-ref name 'use "") "official"))
            names))
  (cond
    [(> (length official-names) 0)
     (list (make-patient-greeting-card (car official-names)))]
    [(> (length names) 0)
     (list (make-patient-greeting-card (car names)))]
    [else '()]))
