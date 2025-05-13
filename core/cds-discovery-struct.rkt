#lang racket/base

;;; =============================== Exports ====================================

(provide make-cds-discovery
         cds-discovery-hook
         cds-discovery-id
         cds-discovery->jsexpr)

;;; ============================ CDS discovery =================================

(struct cds-discovery
  (hook title description id prefetch requirements)
  #:transparent)

(define (make-cds-discovery hook id description
                            #:title [title #f]
                            #:prefetch [prefetch #f]
                            #:requirements [requirements #f])
  (cds-discovery hook
                 title
                 description
                 id
                 prefetch
                 requirements))

;;; ========================== Jsexpr conversion ===============================

(define (cds-discovery->jsexpr discovery)
  (for/hasheq ([k (in-list (list
                            'hook
                            'title
                            'description
                            'id
                            'prefetch
                            'usageRequirements))]
               [v (in-list (list
                            (symbol->string
                             (cds-discovery-hook discovery))
                            (cds-discovery-title discovery)
                            (cds-discovery-description discovery)
                            (symbol->string
                             (cds-discovery-id discovery))
                            (cds-discovery-prefetch discovery)
                            (cds-discovery-requirements discovery)))]
               #:when v)
    (values k v)))
