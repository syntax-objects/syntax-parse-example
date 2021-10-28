#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/defines/defines)]

@(define defines-eval
   (make-base-eval '(require syntax-parse-example/defines/defines)))

@title{@tt{defines}}
@stxbee2021["Fictitious-Rotor" 7]

@; =============================================================================

@defmodule[syntax-parse-example/defines/defines]{}

@defform[(defines [id-spec expr] ...)
         #:grammar ([id-spec id (id ...)])]{
  Make a sequence of definitions --- similar to @racket[let*].

  Definitions are automatically dispatched either to @racket[define] or @racket[define-values]
  as appropriate.

  @examples[#:eval defines-eval
    (defines
      [x 4]
      [y 18]
      [(quot rem) (quotient/remainder x y)])
    quot
    rem
  ]

  The macro uses the @racket[~?] fallthrough syntax to choose between
  @racket[define] and @racket[define-values].

  @racketfile{defines.rkt}

  @; TODO add description here
}
