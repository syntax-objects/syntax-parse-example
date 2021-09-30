#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/define-curry/define-curry)]

@(define define-curry-eval
   (make-base-eval '(require racket/string syntax-parse-example/define-curry/define-curry)))

@title{@tt{define/curry}}
@stxbee2021["agj" 5]

@; =============================================================================

@defmodule[syntax-parse-example/define-curry/define-curry]{}

@defform[(define/curry (fn-id arg-id ...+) body ...+)]{
  Defines an automatically currying procedure.

  @examples[#:eval define-curry-eval
    (define/curry (insert-between mid left right)
      (string-join (list left mid right) ""))
    (define dash-between (insert-between "-"))
    (dash-between "left" "right")
  ]

  The macro uses @racket[syntax/loc] to give newly-defined functions
  names that point to their definition rather than to the macro body.

  @racketfile{define-curry.rkt}

}
