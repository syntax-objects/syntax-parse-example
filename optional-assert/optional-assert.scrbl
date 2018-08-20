#lang syntax-parse-example
@require[
  (for-label racket/base racket/contract syntax/parse syntax-parse-example/optional-assert/optional-assert)]

@(define optional-assert-eval
   (make-base-eval '(require syntax-parse-example/optional-assert/optional-assert)))

@title{@tt{optional-assert}}

@; =============================================================================

@defmodule[syntax-parse-example/optional-assert/optional-assert]{}

@defform[(optional-assert expr ...)]{
  The @racket[optional-assert] form expands to either:
  @itemlist[
  @item{
    a test that evaluates an expression and halts the program if the result is @racket[#f],
  }
  @item{
    or nothing
  }
  ]
  depending on the compile-time value of the environment variable @litchar{DISABLE_OPTIONAL_ASSERT}.

  @racketfile{optional-assert.rkt}

}

@defproc[(exn:fail:optional-assert? [x any/c]) boolean?]{
  Predicate for an assertion failure.
}
