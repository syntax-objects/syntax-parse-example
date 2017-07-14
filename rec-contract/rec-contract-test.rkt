#lang racket/base
(module+ test
  (require rackunit racket/contract syntax-parse-example/rec-contract/rec-contract)

  (define/contract (deep n)
    (-> integer? (rec/c t (or/c integer? (list/c t))))
    (if (zero? n)
      n
      (list (deep (- n 1)))))

  (check-equal? (deep 4) '((((0)))))
  (check-equal? (deep 0) 0)

  (define/contract (bad-deep n)
    (-> integer? (rec/c t (or/c integer? (list/c t))))
    (if (zero? n)
      n
      (list 1 (bad-deep (- n 1)))))

  (check-equal? (bad-deep 0) 0)

  (check-exn exn:fail:contract?
    (λ () (bad-deep 1)))

)
