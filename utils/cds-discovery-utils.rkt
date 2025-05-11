#lang racket/base

;;; =============================== Imports ====================================

;;; =============================== Exports ====================================

(provide make-cds-service-discovery
         cds-service-discovery-hook
         cds-service-discovery-id
         cds-service-discovery->jsexpr)

;;; ===================== CDS service discovery struct =========================

(struct cds-service-discovery
  (hook title description id prefetch requirements)
  #:transparent)

(define (make-cds-service-discovery hook id description
                                    #:title [title #f]
                                    #:prefetch [prefetch #f]
                                    #:requirements [requirements #f])
  (cds-service-discovery hook
                         title
                         description
                         id
                         prefetch
                         requirements))

;;; ========================== Jsexpr conversion ===============================

(define (cds-service-discovery->jsexpr discovery)
  (for/hasheq ([k (in-list (list
                            'hook
                            'title
                            'description
                            'id
                            'prefetch
                            'usageRequirements))]
               [v (in-list (list
                            (symbol->string
                             (cds-service-discovery-hook discovery))
                            (cds-service-discovery-title discovery)
                            (cds-service-discovery-description discovery)
                            (symbol->string
                             (cds-service-discovery-id discovery))
                            (cds-service-discovery-prefetch discovery)
                            (cds-service-discovery-requirements discovery)))]
               #:when v)
    (values k v)))
