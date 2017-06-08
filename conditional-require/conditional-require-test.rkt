#lang racket/base
(module+ test
  (require rackunit syntax-parse-example/conditional-require/conditional-require
           syntax/macro-testing)

  (conditional-require #true
    racket/list
    racket/match)

  (check-pred values partition)

  (conditional-require #false
    racket/math
    racket/flonum)

  (check-pred values flabs)

  (check-exn exn:fail:syntax?
    (λ ()
      (convert-compile-time-error
        (conditional-require 1 racket/unsafe/ops typed/racket))))

  (check-exn exn:fail:syntax?
    (λ ()
      (convert-compile-time-error
        (conditional-require #false 1 2))))

)
