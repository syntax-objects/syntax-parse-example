#lang racket/base
(module+ test
  (require rackunit racket/port syntax-parse-example/log-once/log-once)

  (check-equal?
    (with-output-to-string
      (lambda ()
        (for ([char (in-string "abcdefghijklmnopqrstuvwxyz")])
          (log-once #:skip-count 18
                    #:log-count 3
                    char))))
    "char = #\\s. \nchar = #\\t. \nchar = #\\u. \n")

  (check-equal?
    (with-output-to-string
      (lambda ()
        (for ([char (in-string "abcdefGhijkLmnopQrstuVwxyz")])
          (log-once #:skip-count 2
                    #:when (char-upper-case? char)
                    char))))
    "char = #\\Q. \n")
)
