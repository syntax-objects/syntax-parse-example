#lang racket/base
(module+ test
  (require rackunit
           (only-in racket/function thunk)
           (only-in racket/port with-output-to-string)
           syntax-parse-example/try-catch-finally/1)

  (check-equal?
    (try 1)
    1)

  (check-equal?
    (try (/ 1 0)
         (catch [exn:fail? e (exn-message e)]))
    "/: division by zero")

  (check-equal?
    (with-output-to-string
      (thunk
        (check-equal?
          (try 1
               (finally (displayln "cleaning up")))
          1)))
    "cleaning up\n")

  (check-equal?
    (with-output-to-string
      (thunk
        (check-equal?
          (try (/ 1 0)
               (catch [exn:fail? _ 0])
               (finally (displayln "cleaning up")))
          0)))
    "cleaning up\n")

  (check-equal?
    (try (/ 1 0)
         (catch/match [(? exn:fail? e) (exn-message e)]))
    "/: division by zero")

  (struct posn [x y])
  (check-equal?
    (try (raise (posn 1 2))
         (catch/match [(posn 1 y) y]))
    2)
  (check-equal?
    (try (raise (posn 1 2))
         (catch [exn? e (exn-message e)])
         (catch/match [(posn 1 y) y]))
    2)

  (check-equal?
    (with-output-to-string
      (thunk
        (check-equal?
          (try 1
               (finally (displayln "cleaning up")))
          1)))
    "cleaning up\n")

  (check-equal?
    (with-output-to-string
      (thunk
        (check-equal?
          (try (/ 1 0)
               (catch/match [(? exn:fail?) 0])
               (finally (displayln "cleaning up")))
          0)))
    "cleaning up\n"))