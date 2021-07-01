#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/let-star/let-star)]

@(define let-star-eval
   (make-base-eval '(require syntax-parse-example/let-star/let-star)))

@title{@tt{let-star}}

@; =============================================================================

@defmodule[syntax-parse-example/let-star/let-star]{}

@defform[(let-star ((id expr) ...) expr)]{

  Racket's @racket[let] binds identifiers simultaneously;
   Racket's @racket[let*] binds identifiers in sequence.
  For example:

  @racketblock[
    (let* ([a 1]
           [b (+ a 1)])
      b)]

  behaves the same as a nested @racket[let]:

  @racketblock[
    (let ([a 1])
      (let ([b (+ a 1)])
        b))]

  The @racket[let-star] macro implements @racket[let*] in terms of @racket[let].

  @racketfile{let-star.rkt}

  Note:
  @itemlist[
  @item{
    The macro is recursive.
    The use of @racket[let-star] in the second clause will later expand to
     a sequence of @racket[let]s.
  }
  @item{
    The pattern @racket[...+] matches one or more of the previous pattern.
  }
  ]

  @examples[#:eval let-star-eval
   (eval:error (let-star 1))

   (eval:error (let-star ([a 1])))

   (let-star ([a 1]
              [b (+ a 1)]
              [c (+ b 1)])
     c)
  ]
}
