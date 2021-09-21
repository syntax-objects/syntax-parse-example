#lang syntax-parse-example
@require[
  (for-label racket/base racket/syntax syntax/transformer syntax/parse syntax-parse-example/fresh-variable/fresh-variable)]

@(define fresh-variable-eval
   (make-base-eval '(require (for-syntax racket/base syntax/parse syntax/transformer syntax-parse-example/fresh-variable/fresh-variable))))

@title{Generate Temporaries On The Fly: @tt{fresh-variable} Syntax Class}
@stxbee2021["shhyou" 22]

@; =============================================================================

@defmodule[syntax-parse-example/fresh-variable/fresh-variable]{}

Some macros need to generate a sequence of fresh identifiers corresponding to a
list of input forms. The standard solution is to invoke @racket[generate-temporaries]
with a syntax list and bind the result to a new pattern variable. However, the
new pattern variable is disconnected from the input forms and such an approach
quickly becomes unmanageable when the input forms come nested in more than one
ellipses.

The @racket[fresh-variable] syntax class solves both these issues.
First, it tightly couples generated identifiers to the input forms.
The new identifiers even have DrRacket binding arrows.
Second, it leverages the @racket[syntax-parse] pattern matcher to handle
deeply-nested repetitions.

@defidform[fresh-variable]{
  Syntax class that binds an attribute @racket[_fresh-var] to a fresh temporary
  variable.

  In the example below, we create a macro @racket[define/immutable-parameter]
  for defining functions whose parameters cannot be mutated by the function
  body.
  The macro parses arguments using the @racket[fresh-variable] syntax class
  to generate temporary identifiers on the fly.

  @examples[#:eval fresh-variable-eval
    (define-syntax (define/immutable-parameter stx)
      (syntax-parse stx
        [(_ (name:id (~or* (~and _:id arg:fresh-variable)
                           [(~and _:id arg:fresh-variable) default-value:expr])
                     ...)
            body:expr ...+)
         #'(define (name (~? [arg.fresh-var default-value]
                             arg.fresh-var) ...)
             ;; disable set! on arg
             (define-syntax arg
               (make-variable-like-transformer #'arg.fresh-var #f))
             ...
             body ...)]))

    (define/immutable-parameter (fib n [verbose? #f])
      (code:comment "(set! n 12345) ;=> syntax error")
      (when verbose?
        (printf "(fib ~a)\n" n))
      (cond
        [(<= n 1) n]
        [else
         (+ (fib (- n 1) verbose?)
            (fib (- n 2) verbose?))]))

    (fib 5)
  ]

  The implementation accepts any expression and generates a temporary identifier.

  @racketfile{fresh-variable.rkt}

  @itemize[
    @item{
      @bold{Q.} Why use @racket[_name] instead of asking for an identifier with @racket[_name:id]?
    }
    @item{
      @bold{A.} Some macros may let-bind subforms to first evaluate them for
      later use. Therefore the subforms can be any expressions. I couldn't find
      a way to pass syntax classes around or compose them, so the @racket[_:id]
      specification is left out of the syntax class.
    }
  ]
}
