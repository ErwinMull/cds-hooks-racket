#lang racket/base

;;; =============================== Imports ====================================

(require racket/list)

;;; =============================== Exports ====================================

(provide fhirpath-lite)

;;; ============================ FHIRPath lite =================================

(define (fhirpath-proc res proc parameters)
  (case proc
    [(where) (filter (λ (e)
                       (equal? (hash-ref e (car parameters))
                               (cadr parameters)))
                     res)]))

(define (fhirpath-lite res . parameters)
  (cond
    [(or (null? parameters) (null? res)) res]
    [(symbol? (car parameters))
     (apply fhirpath-lite
            (hash-ref res (car parameters) '())
            (cdr parameters))]
    [(list? (car parameters))
     (let ([tmp (fhirpath-proc res (caar parameters) (cdar parameters))])
       (append (map (λ (x) (apply fhirpath-lite x (cdr parameters)))
                    tmp)))]))
