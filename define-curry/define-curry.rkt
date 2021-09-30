#lang racket
(provide define/curry)
(require (for-syntax syntax/parse))

(begin-for-syntax
  (define-syntax-class name-params
    #:description "name and parameters clause"
    (pattern (name:id params:id ...+)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(params ...)))
             "duplicate parameter name")))

(define-syntax (define/curry stx)
  (syntax-parse stx
    [(_ np:name-params body ...+)
     #`(define np.name
         (curry
           #,(syntax/loc stx
               (λ (np.params ...) body ...))))]))

