#lang racket/base
(module+ test
  (require rackunit syntax-parse-example/optional-assert/optional-assert)

  (check-not-exn
    (lambda () (optional-assert (= (+ 2 2) 4))))

  (check-exn (lambda (e)
               (and (exn:fail:optional-assert? e)
                    (regexp-match? #rx"\\(= \\(\\+ 2 2\\) 5\\)"
                                   (exn-message e))))
    (lambda () (optional-assert (= (+ 2 2) 5))))
)
