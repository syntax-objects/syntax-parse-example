#lang syntax-parse-examples
@require[
  (for-label
    racket/base
    syntax/parse
    syntax-parse-examples/first-class-or/first-class-or
    syntax-parse-examples/first-class-or/first-class-or-test)]

@(define first-class-or-eval
   (make-base-eval
     '(require syntax-parse-examples/first-class-or/first-class-or)))

@title{@tt{first-class-or}}
@; =============================================================================

Racket's @racket[or] is a macro, not a function.
It cannot be used like a normal value (i.e., evaluated as an identifier).
@margin-note{See also @secref["and+or" #:doc '(lib "scribblings/guide/guide.scrbl")]}

@examples[#:eval (make-base-eval)
  (or #false #true)
  (eval:error (apply or '(#false #true)))
]

The following @tech/guide{identifier macro} works like Racket's @racket[or]
 when called like a function, and evaluates to a function when it appears in
 an identifier postion.
The function it evaluates to is @emph{similar} to @racket[or], but evaluates
 both its arguments.

@racketfile{first-class-or/first-class-or.rkt}

@examples[#:eval first-class-or-eval
  (first-class-or #false #true)
  (first-class-or (+ 2 3) (let loop () (loop)))
  (map first-class-or '(9 #false 3) '(8 #false #false))
]

See @racketmodname[syntax-parse-examples/first-class-or/first-class-or-test]
 for more examples.
