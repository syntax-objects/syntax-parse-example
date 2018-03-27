#lang syntax-parse-example
@require[
  (for-label racket/base rackunit syntax/parse syntax-parse-example/multi-check-true/multi-check-true)]

@(define multi-check-true-eval
   (make-base-eval '(require syntax-parse-example/multi-check-true/multi-check-true)))

@title{@tt{multi-check-true}}

@; =============================================================================

@defmodule[syntax-parse-example/multi-check-true/multi-check-true]{}

@defform[(multi-check-true expr ...)]{

  The @tt{multi-check-true} expands into a sequence of @racket[check-true] unit tests.
  For example:

  @racketblock[
    (multi-check-true
      #true
      #false
      (even? 0))]

  expands to code that behaves the same as:

  @racketblock[
    (check-true #true)
    (check-true #false)
    (check-true (even? 0))]

  The main difference between the macro and the example is that the macro uses
   @racket[with-check-info*] to improve test failure messages.
  If part of a @racket[multi-check-true] fails, the error message points
   to the bad expression (rather than the @racket[multi-check-true] macro).

  @racketfile{multi-check-true.rkt}
}
