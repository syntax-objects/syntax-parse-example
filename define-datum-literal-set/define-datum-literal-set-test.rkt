#lang racket/base
(module+ test
  (require rackunit syntax-parse-example/define-datum-literal-set/define-datum-literal-set
           (for-syntax racket/base syntax/parse))

  (define-datum-literal-set yes-ish
    (yes y Y maybe probably i-think-so))

  (define-syntax (yes->true stx)
    (syntax-parse stx
     [(_ y:yes-ish)
      #'#true]
     [(_ _)
      #'#false]))

  (check-true (yes->true yes))
  (check-true (yes->true probably))

  (check-false (yes->true no))
  (check-false (yes->true 3))

  (define-datum-literal-set C-keyword
    (auto break case char const continue default do double else))

  (define-syntax (is-C-keyword? stx)
    (syntax-parse stx
     [(_ x:C-keyword)
      #'#true]
     [(_ x)
      #'#false]))

  (check-true (is-C-keyword? else))
  (check-false (is-C-keyword? synchronized))
)
