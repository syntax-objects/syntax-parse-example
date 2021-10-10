#lang racket/base
(provide pyret-for)

(require racket/match syntax/parse/define (for-syntax racket/base))

(define-syntax-parse-rule
  (pyret-for f:expr ([pat:expr arg:expr] ...) body:expr ...+)
  #:with (x ...) (generate-temporaries (attribute arg)) 
  (f (λ (x ...)
       (match-define pat x) ...
       body ...)
     arg ...))

