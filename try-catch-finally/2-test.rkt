#lang racket/base
(module+ test
  (require rackunit
           (only-in racket/port with-output-to-string)
           syntax-parse-example/try-catch-finally/2)

  (test-case "catch"
    (check-equal?
      (try
        (raise-syntax-error #f "a syntax error")
        (catch (exn:fail:syntax? e)
          "got a syntax error"))
      "got a syntax error"))

  (test-case "cc"
    (check-equal?
      (with-output-to-string
        (lambda ()
          (let/cc up
            (try
              (displayln "at before")
              (up (void))
              (displayln "at after")
              (finally (displayln "out"))))))
      "at before\nout\n"))

)
