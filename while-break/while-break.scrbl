#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/while-break/while-break)]

@(define while-break-eval
   (make-base-eval '(require syntax-parse-example/while-break/while-break)))

@title{Basic while loop equipped with break}
@stxbee2021["countvajhula" 20]

@; =============================================================================

@defmodule[syntax-parse-example/while-break/while-break]{}

@defform[(while test body ...+)]{

  @examples[#:eval while-break-eval
    (define x 5)

    (while (> x 0)
      (displayln x)
      (set! x (sub1 x)))

    (set! x 5)

    (while #t
      (displayln x)
      (set! x (sub1 x))
      (unless (> x 0)
        (break)))
  ]


  @; TODO add description here
}

@defidform[break]{
  May only appear within an @racket[while] form.
}

The macro uses an escape continuation to provide the semantics of @racket[break], and
leverages it using a syntax parameter so that the continuation is accessible
in the lexical scope of the @racket[while] body, and also so that @racket[break] is a syntax
error outside the @racket[while] loop.

@racketfile{while-break.rkt}
