#lang racket/base
(provide multi-check-true)
(require rackunit (for-syntax racket/base syntax/srcloc syntax/parse))

(define-syntax (multi-check-true stx)
  (syntax-parse stx
   [(_ e* ...)
    #`(begin
        #,@(for/list ([e (in-list (syntax-e #'(e* ...)))])
             (define loc (build-source-location-list e))
             #`(with-check-info* (list (make-check-location '#,loc))
                 (λ () (check-true #,e)))))]))
