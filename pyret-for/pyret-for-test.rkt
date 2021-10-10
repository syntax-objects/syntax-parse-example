#lang racket/base
(module+ test
  (require rackunit racket/list racket/string syntax-parse-example/pyret-for/pyret-for)

  (test-case "no-match"
    (define things     '(("pen") ("pineapple") ("apple") ("pen")))
    (define quantities '(1       2             3         5))
    (check-true
      (pyret-for andmap ([thing things] [quantity quantities])
        (or (string-contains? (first thing) "apple")
            (odd? quantity)))))

  (test-case "match"
    (define things     '(("pen") ("pineapple") ("apple") ("pen")))
    (define quantities '(1       2             3         5))
    (check-true
      (pyret-for andmap ([(list thing) things] [quantity quantities])
        (or (string-contains? thing "apple")
            (odd? quantity)))))
)
