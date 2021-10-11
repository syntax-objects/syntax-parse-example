#lang syntax-parse-example
@require[
  (for-label racket/base racket/match syntax/parse syntax-parse-example/define-extend/define-extend)]

@(define define-extend-eval
   (make-base-eval '(require racket/match syntax-parse-example/define-extend/define-extend)))

@title{@tt{define-extend}}
@stxbee2021["camoy" 13]

@; =============================================================================

Suppose we're writing interpreters @racket[_interp0] and @racket[_interp1] for
languages @racket[_L0] and @racket[_L1] respectively.
@racket[_L0] has numbers and binary addition, and @racket[_L1] extends @racket[_L0]
with binary multiplication.
Goal: Write @racket[_interp1] without copying all the cases from @racket[_interp0].


@bold{Basic Solution}

One solution is to write the interpreters in open-recursive style. Instead of
recurring directly, recursive calls occur indirectly through an extra
parameter. An interpreter can be invoked by closing the recursion using a
fixed-point combinator.

@racketblock[
(define fix
  (λ (f)
    ((λ (x) (f (λ (g) ((x x) g))))
     (λ (x) (f (λ (g) ((x x) g)))))))

(define ((interp0 recur) e)
  (match e
    [`(+ ,x ,y) (+ (recur x) (recur y))]
    [(? number?) e]))

((fix interp0) '(+ (+ 1 2) (+ 5 6)))

(define ((interp1 recur) e)
  (match e
    [`(* ,x ,y) (* (recur x) (recur y))]
    [_ ((interp0 recur) e)]))

((fix interp1) '(+ (+ 1 2) (* 5 6)))
]

We can do better.


@defmodule[syntax-parse-example/define-extend/define-extend]{}

@defform[(define-extend (name . formals) maybe-extend body ...+)
         #:grammar ([maybe-extend (code:line) (#:extend parent-id)])]{
  The @racket[define-extend] macro allows you to write extensible procedures in a
  more natural style.

  @examples[#:eval define-extend-eval
    (define-extend (interp0 e)
      (match e
        [`(+ ,x ,y) (+ (interp0 x) (interp0 y))]
        [(? number?) e]))

    (interp0 '(+ (+ 1 2) (+ 5 6)))

    (define-extend (interp1 e)
      #:extend interp0
      (match e
        [`(* ,x ,y) (* (interp1 x) (interp1 y))]
        [_ (interp0 e)]))

    (interp1 '(+ (+ 1 2) (* 5 6)))
  ]

  This macro supports some static checking. If the procedure we're extending
  wasn't defined using @racket[define-extend], then we get a compile-time error.

  @examples[#:eval define-extend-eval
    (eval:error
      (define-extend (interp1 e)
        #:extend map
        'not-implemented))]

  Implementation:

  @racketfile{define-extend.rkt}

  For a valid input, @racket[define-extend] generates two variants of the procedure: a
  closed version and an open version. It then creates a transformer binding
  that records the name of both these variants in an extensible struct. This
  struct has @racket[prop:rename-transformer] so that calling the procedure defaults to
  the closed variant.

  When defining an extension of procedure @racket[_f], we make sure to shadow the binding
  of @racket[_f] within the body of the extension so as to close it off appropriately. We
  use the extensible struct (found by @racket[syntax-local-value/immediate]) to get the
  identifier of the open version of @racket[_f].

}
