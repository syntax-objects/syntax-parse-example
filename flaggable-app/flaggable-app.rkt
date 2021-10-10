#lang racket/base
(provide #%app)

(require syntax/parse/define
         (only-in racket [#%app racket:#%app])
         (for-syntax racket/base))

(begin-for-syntax
  (define-splicing-syntax-class arg/keyword
    #:attributes (k v)
    ;; first case: something like #:a 1
    (pattern {~seq k:keyword v:expr})
    ;; second case: something like #:a.
    (pattern {~seq k:keyword}
             #:with v #'#t)))

(define-syntax-parse-rule
  (#%app f arg/no-keyword:expr ... arg/keyword:arg/keyword ...)
  (racket:#%app f arg/no-keyword ... {~@ arg/keyword.k arg/keyword.v} ...))

