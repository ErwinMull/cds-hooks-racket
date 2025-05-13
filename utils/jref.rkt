#lang typed/racket/base

;;; =============================== Exports ====================================

(provide jref)

;;; ================================ Types =====================================

(define-type Jstring String)
(define-type Jnumber Real)
(define-type Jnull 'null)
(define-type Jbool Boolean)
(define-type Jarray (Listof Jsexpr))
(define-type Jobject (Immutable-HashTable Symbol Jsexpr))

(define-type Jsexpr (U Jstring Jnumber Jnull Jbool Jarray Jobject))

;;; ========================== Jsexpr referencing ==============================

(: jref
   (->* (Jsexpr (Listof (U Nonnegative-Integer Symbol)))
        (Any)
        (U Jsexpr Any)))
(define (jref js refs [default '()])
  (cond
    [(null? refs) js]
    [(and (list? js) (integer? (car refs)))
     (let ([index (car refs)]
           [len (length js)])
       (if (< index len)
           (jref (list-ref js index) (cdr refs) default)
           default))]
    [(and (hash? js) (symbol? (car refs)))
     (let ([key (car refs)])
       (if (hash-has-key? js key)
           (jref (hash-ref js key) (cdr refs) default)
           default))]
    [else default]))
