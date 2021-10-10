#lang racket/base
(module+ test
  (require rackunit syntax-parse-example/js-dict/js-dict)

  (test-begin
    (define d 4)
    (define base-1 (js-dict [x '((10))] [b 20]))
    (define base-2 (js-dict [y 30] [a 40]))
    (define obj
      (js-dict
       [a 1]
       #:merge base-1
       [b 2]
       #:merge base-2
       [#:expr (string->symbol "c") 3]
       d))

    (test-case "js-dict"
      (check-equal? obj '#hash((a . 40) (b . 2) (c . 3) (d . 4) (x . ((10))) (y . 30))))

    (test-case "js-extract"
      (js-extract ([#:expr (string->symbol "a") f]
               c
               d
               [x (list (list x))]
               #:rest rst)
              obj)
      (check-equal? f  40)
      (check-equal? c 3)
      (check-equal? d 4)
      (check-equal? x 10)
      (check-equal? rst '#hash((b . 2) (y . 30)))))
)
