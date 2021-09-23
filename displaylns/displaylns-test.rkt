#lang racket/base
(module+ test
  (require rackunit racket/port syntax-parse-example/displaylns/displaylns)

  (check-equal?
    (with-output-to-string
      (lambda ()
        (displayln* 1 2 3 4 5 '(this is a list of datums 1 2 3 "hi"))))
    "1 2 3 4 5 (this is a list of datums 1 2 3 hi)\n")

)
