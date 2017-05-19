#lang racket/base
(provide define-for-macros get-macroed)
(require (for-syntax racket/base syntax/parse))

(define-syntax (define-for-macros stx)
  (syntax-parse stx
    [(_ name:id expr)
     #'(define-syntax name expr)]))

(define-syntax (get-macroed stx)
  (syntax-parse stx
    [(_ name:id)
     #`(#%datum . #,(syntax-local-value #'name))]))

