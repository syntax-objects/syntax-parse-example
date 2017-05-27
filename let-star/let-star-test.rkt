#lang racket/base
(module+ test
  (require rackunit syntax/macro-testing syntax-parse-example/let-star/let-star)

  (check-equal?
    (let-star ([x 1] [x 2]) x)
    2)

  (check-equal?
    (let-star ([x 0] [y (+ x 1)] [z (+ y 1)]) z)
    2)

  (check-equal?
    (let-star ([a 1] [b 2]) a b)
    2)

  (check-exn exn:fail:syntax?
    (λ () (convert-compile-time-error (let-star 3 4))))

  (check-exn exn:fail:syntax?
    (λ () (convert-compile-time-error (let-star ()))))

  (check-exn exn:fail:syntax?
    (λ () (convert-compile-time-error (let-star ([x 1] [y 2])))))
)
