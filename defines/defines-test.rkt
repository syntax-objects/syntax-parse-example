#lang racket/base
(module+ test
  (require rackunit syntax-parse-example/defines/defines)

  (test-case "quot/rem"
    (defines
      [x 4]
      [y 18]
      [(quot rem) (quotient/remainder x y)])
    (check-equal? quot 0)
    (check-equal? rem 4))
)
