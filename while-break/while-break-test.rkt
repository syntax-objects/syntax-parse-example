#lang racket/base
(module+ test
  (require rackunit racket/port syntax-parse-example/while-break/while-break)

  (test-begin
    (define x 5)

    (test-case "ex1"
      (check-equal?
        (with-output-to-string
          (lambda ()
            (while (> x 0)
              (displayln x)
              (set! x (sub1 x)))))
        "5\n4\n3\n2\n1\n"))

    (set! x 5)

    (test-case "ex2"
      (check-equal?
        (with-output-to-string
          (lambda ()
            (while #t
              (displayln x)
              (set! x (sub1 x))
              (unless (> x 0)
                (break)))))
        "5\n4\n3\n2\n1\n")))

)
