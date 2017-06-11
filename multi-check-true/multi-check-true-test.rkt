#lang racket/base
(module+ test
  (require rackunit syntax-parse-example/multi-check-true/multi-check-true)

  (multi-check-true
   #t
   (and #true #true)
   (or #false #true)
   (= 4 (+ 2 2)))
)
