#lang racket/base
(module+ test
  (require rackunit syntax-parse-examples/higher-order-or/higher-order-or racket/port)

  (check-equal?
    (higher-order-or 1 2 3)
    1)

  (check-equal?
    (higher-order-or 'done (let loop-forever () (loop-forever)))
    'done)

  (check-equal?
    (string-length
      (with-output-to-string
        (λ ()
          (check-equal? (higher-order-or (newline)) (void)))))
    1)

  (check-equal?
    (apply higher-order-or '(#false #true 2))
    #true)
)
