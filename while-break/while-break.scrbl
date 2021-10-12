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

  A simple while loop familiar from many programming languages, this macro
  works by using a
  @seclink["Named_let" #:doc '(lib "scribblings/guide/guide.scrbl")]{named let}
  to provide the looping, and an @tech/reference{escape continuation} to
  provide the ability to @racket[break] out of the loop.

  In typical implementations, @racket[break] is a statement that allows you to
  leave the while loop at any time, but it is also a special kind of statement
  in that you can only use it within the body of the while loop. To ensure this
  behavior, the macro defines @racket[break] as a
  @seclink["stxparam" #:doc '(lib "scribblings/reference/reference.scrbl")]{syntax parameter},
  which in a nutshell is a way to define syntax with default behavior outside the macro
  body (in our case, simply reporting a syntax error) and special behavior
  within the macro body. This parameter is then used as a label for the
  continuation so that it may be used to exit the loop.
}

@defform[(break value ...)]{
  This is an alias for the continuation recorded at the start of the loop.
  Invoking it calls the continuation with the provided value (if any) which
  becomes the value of the entire @racket[while] expression. It may only appear
  within a @racket[while] form, and is a syntax error outside of one.
}

@racketfile{while-break.rkt}

In Racket, most expressions evaluate to a specific value (or many values),
reflecting the functional style of programming which discourages side effects
such as mutating the value of a variable. A while loop inherently encourages
side effects since it typically has no return value (and of course, must mutate
a variable if the loop is to terminate!). Yet, in the present implementation,
the while expression can evaluate to a specific value when one is explicitly
returned via @racket[break]. Could the implementation be modified to return a
value even in the case of a normal exit? For instance, we could have it return
the value of the final expression executed in the course of looping. As an
exercise, see if you can make the while loop a little more "functional" by
allowing it to always return a value.

