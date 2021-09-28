#lang racket
(provide marc-matcher (struct-out marc-subfield))

(require syntax/parse/define
         syntax-parse-example/marc-matcher/marc-matcher-helpers
         (for-syntax syntax-parse-example/marc-matcher/marc-matcher-syntax-classes))

(define-syntax (marc-matcher stx)
  (syntax-parse stx
    [(_ (var:marc-var-defn ...) body:expr ...)
     (define params #'(var.name ...))
     (define regexps #'(var.re ...))
     #`(λ (input [sep "$"])
         (define args (get-subfield-data '#,regexps input sep))
         (apply (λ #,params (begin body ...)) (map simplify-groups args)))]))
