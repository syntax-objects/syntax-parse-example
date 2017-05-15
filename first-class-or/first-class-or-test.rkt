#lang racket/base
(module+ test
  (require rackunit syntax-parse-examples/first-class-or/first-class-or racket/port)

  (check-equal?
    (first-class-or 1 2 3)
    1)

  (check-equal?
    (first-class-or 'done (let loop-forever () (loop-forever)))
    'done)

  (check-equal?
    (string-length
      (with-output-to-string
        (λ ()
          (check-equal? (first-class-or (newline)) (void)))))
    1)

  (check-equal?
    (apply first-class-or '(#false #true 2))
    #true)
)
