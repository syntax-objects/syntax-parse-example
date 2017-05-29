#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/def/def)]

@(define def-eval
   (make-base-eval '(require syntax-parse-example/def/def)))

@title{@tt{def}}

@; =============================================================================

@examples[#:eval def-eval
 (module snoc racket/base
   (require syntax-parse-example/def/def)
   (def (snoc (x* : list?) x)
     "Append the value `x` to the end of the given list"
     #:test [
       ((snoc '(1 2) 3) ==> '(1 2 3))
       ((snoc '(a b) '(c)) ==> '(a b (c)))]
     (append x* (list x)))
   (provide snoc))
 (require 'snoc)
 (eval:error (snoc 1 '(2 3)))
 (snoc '(1 2) 3)
]

The @racket[def] macro is similar to @racket[define] but:
@itemlist[
@item{
  requires a docstring
}
@item{
  requires test cases;
}
@item{
  optionally accepts contract annotations on its arguments; and
}
@item{
  optionally accepts pre- and post- conditions.
}
]

@examples[#:eval def-eval
  (module gcd racket/base
    (require syntax-parse-example/def/def)
    (def (gcd (x : integer?) (y : integer?))
      "greatest common divisor"
      #:pre [
        (>= "First argument must be greater-or-equal than second")]
      #:test [
        ((gcd 10 3) ==> 1)
        ((gcd 12 3) ==> 3)]
      (cond
       [(zero? y) x]
       [else (gcd y (- x (* y (quotient x y))))]))
     (provide gcd))
  (require 'gcd)
  (eval:error (gcd 42 777))
  (gcd 777 42)
]

If the docstring or test cases are missing, @racket[def] throws a syntax error.

@examples[#:eval def-eval
  (eval:error (def (f x)
                x))
  (eval:error (def (f x)
                "identity"
                x))
  (eval:error (def (f x)
                #:test [((f 1) ==> 1)]
                x))
]

How to read the macro:
@itemlist[#:style 'ordered
@item{
  The @racket[begin-for-syntax] defines two syntax classes (see @secref["Syntax_Classes" #:doc '(lib "syntax/scribblings/syntax.scrbl")]).
  The first syntax class, @racket[arg-spec], captures arguments with an optional contract annotation.
  The second, @racket[doc-spec], captures docstrings.
}
@item{
  The large @racket[~or] pattern captures the required-and-optional stuff that
   @racket[def] accepts---in particular, the docstring, the @racket[#:test] test cases,
   the @racket[#:pre] pre-conditions, and the @racket[#:post] post-conditions.
}
@item{
  The four @racket[#:with] clauses build syntax objects that run unit tests
   and/or checks.
}
]

The @racket[def] macro:

@racketfile{def.rkt}

Notes:
@itemlist[
@item{
  This macro gives poor error messages when the docstring or test
   cases are missing.
}
@item{
  The @racket[doc-spec] syntax class could be extended to accept Scribble, or
   another kind of docstring syntax.
}
@item{
  A @racket[#:test] case may optionally use the @racket[#:stdout] keyword.
  If given, the test will fail unless running the test prints the same string
   to @racket[current-output-port].
}
]
