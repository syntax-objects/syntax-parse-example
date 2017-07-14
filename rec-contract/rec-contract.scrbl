#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/rec-contract/rec-contract)]

@(define rec-contract-eval
   (make-base-eval '(require racket/contract syntax-parse-example/rec-contract/rec-contract)))

@title{@tt{rec/c}}

@; =============================================================================

The @racket[rec/c] macro uses Racket's @racket[recursive-contract] form
 to create anonymous recursive contracts.

@racketfile{rec-contract.rkt}

@examples[#:eval rec-contract-eval
  (define/contract (deep n)
    (-> integer? (rec/c t (or/c integer? (list/c t))))
    (if (zero? n)
      n
      (list (deep (- n 1)))))

  (deep 4)
]


