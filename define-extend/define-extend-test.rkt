#lang racket/base
(module+ test
  (require racket/match rackunit syntax/macro-testing syntax-parse-example/define-extend/define-extend)

  (test-begin

    (define-extend (interp0 e)
      (match e
        [`(+ ,x ,y) (+ (interp0 x) (interp0 y))]
        [(? number?) e]))

    (test-case "interp0"
      (check-equal? (interp0 '(+ (+ 1 2) (+ 5 6))) 14))

    (define-extend (interp1 e)
      #:extend interp0
      (match e
        [`(* ,x ,y) (* (interp1 x) (interp1 y))]
        [_ (interp0 e)]))

    (test-case "interp1"
      (check-equal? (interp1 '(+ (+ 1 2) (* 5 6))) 33))

    (test-case "bad-parent"
      (check-exn exn:fail:syntax?
        (lambda ()
          (convert-compile-time-error (let ()
            (define-extend (interp1 e)
              #:extend map
              'not-implemented)
            (void)))))))
)
