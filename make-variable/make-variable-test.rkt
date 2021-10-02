#lang racket/base
(module+ test
  (require rackunit syntax-parse-example/make-variable/make-variable)

  (test-case "make-variable"
    (check-equal?
      (make-variable "this_variable_will_probably_change")
      "this_variable_will_probably_change=\"this_variable_will_probably_change\"")
    (define Z "Zzz...")
    (check-equal? (make-variable Z) "Z=\"Zzz...\""))

)
