#lang syntax-parse-example
@require[
  (for-label racket/base racket/contract syntax/parse syntax-parse-example/make-variable/make-variable)]

@(define make-variable-eval
   (make-base-eval '(require syntax-parse-example/make-variable/make-variable)))

@title{@tt{make-variable}}
@stxbee2021["xgqt" 8]
@nested[#:style 'inset @emph{Adapted from the @hyperlink["https://gitlab.com/xgqt/racket-ebuild" @tt{racket-ebuild}] project.}]

@; =============================================================================

@defmodule[syntax-parse-example/make-variable/make-variable]{}

@defform[(make-variable v)]{
  Formats a variable declaration for POSIX shell scripts.
  @itemlist[
    @item{When given a string or symbol, the input is the name and value of the new variable.}
    @item{When given an identifier, the identifier name is the variable name and the identifier value is the variable's value.}
  ]

  @examples[#:eval make-variable-eval
    (make-variable "this_variable_will_probably_change")
    (define Z "Zzz...")
    (make-variable Z)
  ]

  The macro uses @racket[syntax-parse] to decide how to process its input.
  It then outputs code that calls the @racket[as-variable] helper function.

  @racketfile{make-variable.rkt}

}

@defproc[(as-variable [lhs any/c] [rhs any/c]) string?]{
  Formats two values into a POSIX variable declaration.
}
