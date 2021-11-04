#lang racket/base
(module+ test
  (require rackunit syntax-parse-example/define-with-datum+/define-with-datum+ syntax/datum syntax/macro-testing)

  (define/with-datum+ (x ((y z ...) ...))
    '("X" (("Y1" "Z11" "Z12")
           ("Y2" "Z21"))))

  (check-equal? (datum x) "X")

  (check-equal?
    (with-datum ([w "W"])
      (datum ((y w) ...)))
    '(("Y1" "W") ("Y2" "W")))

  (check-equal?
    (datum ((^ z ... $) ...))
    '((^ "Z11" "Z12" $) (^ "Z21" $)))

  (check-exn #rx"x: pattern variable cannot be used outside of a template"
    (lambda () (convert-compile-time-error x)))

)
