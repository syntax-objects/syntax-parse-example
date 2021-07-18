#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/except-in-quiet/except-in-quiet)]

@(define except-in-quiet-eval
   (make-base-eval '(require syntax-parse-example/except-in-quiet/except-in-quiet)))

@title{@tt{except-in-quiet}}

@; =============================================================================

@defmodule[syntax-parse-example/except-in-quiet/except-in-quiet]{}

@nested[#:style 'inset @emph{Thanks to Leif Andersen for the original macro.}]

Racket's @racket[except-in] form subtracts a sequence of identifiers from
a source.
If the source does not provide one of the named identifiers, then @racket[except-in]
raises a syntax error.

@examples[#:eval (make-base-eval)
(require (except-in racket/list second))
(first '(1 2 3))
(eval:error (second '(1 2 3)))
(eval:error (require (except-in racket/list snd)))]

@defform[(except-in-quiet expr ...)]{
  Similar to @racket[except-in] but does not raise an error when asked to
  subtract a missing identifier.

  @examples[#:eval except-in-quiet-eval
  (require (except-in-quiet racket/list second))
  (first '(1 2 3))
  (eval:error (second '(1 2 3)))
  (require (except-in-quiet racket/list snd))]

  The macro (or rather, @tech/reference{require transformer}) should work in four steps:

  @itemlist[#:style 'ordered
    @item{
      resolve the required expression to a module,
    }
    @item{
      find all exports from that module,
    }
    @item{
      subtract names from the export list, and
    }
    @item{
      proceed with a call to @racket[expand-import].
    }
  ]

  Steps 2 through 4 are implemented fairly well below.
  Step 1, however, is done in a very simple way.
  It works for basic paths like @tt{racket/base} but nothing more complicated.

  @racketfile{except-in-quiet.rkt}
}


