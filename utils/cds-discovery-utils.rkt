#lang racket/base

;;; =============================== Imports ====================================

;;; =============================== Exports ====================================

(provide make-cds-service-discovery
         make-services-discovery-table
         services-discovery-table->jsexpr)

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

;;; ========================= Services hash table ==============================

(define (get-hook/id/info-table discovery)
  (values (cds-service-discovery-hook discovery)
          (cds-service-discovery-id discovery)
          (for/hasheq
              ([k (in-list (list
                            'title
                            'description
                            'prefetch
                            'usageRequirements))]
               [v (in-list (list
                            (cds-service-discovery-title discovery)
                            (cds-service-discovery-description discovery)
                            (cds-service-discovery-prefetch discovery)
                            (cds-service-discovery-requirements discovery)))]
               #:when v)
            (values k v))))

(define (hasheq-deep-set ht k1 k2 v)
  (if (hash-has-key? ht k1)
      (hash-update ht k1 (λ (deep-ht)
                           (hash-set deep-ht k2 v)))
      (hash-set ht k1
                (hasheq k2 v))))

(define (make-services-discovery-table discovery-servlet-list)
  (foldl (λ (item ht)
           (define discovery (car item))
           (define servlet (cdr item))
           (define-values (hook id info-table)
             (get-hook/id/info-table discovery))
           (define info-table-with-handler (hash-set info-table
                                                     'handler servlet))
           (hasheq-deep-set ht hook id info-table-with-handler))
         (hasheq)
         discovery-servlet-list))

;;; ========================== Jsexpr conversion ===============================

(define (services-discovery-table->jsexpr sdt)
  (hasheq
   'services
   (apply append
          (hash-map sdt
                    (λ (hook services)
                      (define hook-str (symbol->string hook))
                      (hash-map services
                                (λ (id ht)
                                  (define id-str (symbol->string id))
                                  (let* ([ht (hash-remove ht 'handler)]
                                         [ht (hash-set ht 'hook hook-str)]
                                         [ht (hash-set ht 'id id-str)])
                                    ht))))))))
