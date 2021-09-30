#lang racket/base
(module+ test
  (require rackunit racket/string syntax-parse-example/define-curry/define-curry)

  (test-case "insert-between"
    (define/curry (insert-between mid left right)
      (string-join (list left mid right) ""))

    (define dash-between (insert-between "-"))

    (check-equal? (dash-between "left" "right") "left-right")
    (check-equal? (((insert-between "-") "left") "right") "left-right")

    (check-true
      (string-suffix? (symbol->string (object-name insert-between))
                      "6:4"))

    (check-exn #rx"arity mismatch" ;; object name
      (lambda () (insert-between 2 3 3 4 4 44))))
)
