#lang racket/base
(module+ test
  (require rackunit
           (only-in racket/port with-output-to-string)
           (only-in racket/format ~a)
           (only-in racket/string string-split)
           syntax-parse-example/try-catch-finally/3)

  (test-case "try"
    (check-equal?
      (with-output-to-string
        (lambda ()
          (try
            (cons 1)
            (catch (or (? exn? (app exn-message msg))
                       (app ~a msg))
              (displayln (car (string-split msg ";"))))
            (finally (displayln "finally")))))
      "cons: arity mismatch\nfinally\n"))

  (test-case "try-with"
    (check-equal?
      (try-with ([port (open-output-string)])
        (displayln "Hello!" port))
      (void)))

  (test-case "try-with*"
    (check-equal?
      (try-with* ([mk-port open-output-string]
                  [port (mk-port)])
        (displayln "Hello!" port))
      (void)))

)

