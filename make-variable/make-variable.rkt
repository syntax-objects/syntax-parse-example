#lang racket/base
(provide make-variable as-variable)
(require (for-syntax racket/base syntax/parse))

(define (as-variable lhs rhs)
  (format "~a=\"~a\"" lhs rhs))

(define-syntax (make-variable stx)
  (syntax-parse stx
    [(_ name:id)
     #'(as-variable (symbol->string 'name) name)]
    [(_ str:string)
     #'(as-variable str str)]
    [(_ ((~literal quote) sym))
     #'(as-variable (symbol->string 'sym) 'sym)]))

