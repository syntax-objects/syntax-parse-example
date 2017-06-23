#lang racket/base
(provide conditional-require)
(require (for-syntax racket/base syntax/parse))

(begin-for-syntax
  (define-syntax-class mod-name
    (pattern _:id)
    (pattern _:str)))

(define-syntax (conditional-require stx)
  (syntax-parse stx
   [(_ test:boolean r1:mod-name r2:mod-name)
    (if (syntax-e #'test)
      #'(require r1)
      #'(require r2))]))
