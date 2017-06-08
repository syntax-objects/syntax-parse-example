#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/conditional-require/conditional-require)]

@(define conditional-require-eval
   (make-base-eval '(require syntax-parse-example/conditional-require/conditional-require)))

@title{@tt{conditional-require}}

@; =============================================================================

This macro conditionally requires one of two module paths based on a compile-time condition.

@racketfile{conditional-require.rkt}

Notes:
@itemlist[
@item{
  The syntax class @racket[mod-name] matches syntactic strings or identifiers.
  This doesn't guarantee that the second and third argument to @racket[conditional-require]
   are valid module paths, but it rules out nonsense like @racket[(conditional-require #true (+ 2 2) 91)].
}
@item{
  The test could be more interesting.
  It could branch on the value of @racket[current-command-line-arguments],
   or do a @racket[case] based on @racket[system-type].
}
]

