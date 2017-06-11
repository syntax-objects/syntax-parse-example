#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/define-datum-literal-set/define-datum-literal-set)]

@(define define-datum-literal-set-eval
   (make-base-eval '(require syntax-parse-example/define-datum-literal-set/define-datum-literal-set (for-syntax racket/base syntax/parse))))

@title{@tt{define-datum-literal-set}}

@; =============================================================================

@racket[syntax-parse] can match literal symbols using the @racket[#:datum-literals]
 option or the @racket[~datum] pattern form.
These work well for a small number of literals.

Given a sequence of symbols, the @racket[define-datum-literal-set] macro builds
 a @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{syntax class} that matches these symbols.

@racketblock[
 (define-datum-literal-set C-keyword
   (auto break case char const continue default do double else))

 (define-syntax (is-C-keyword? stx)
   (syntax-parse stx
    [(_ x:C-keyword)
     #'#true]
    [(_ x)
     #'#false]))

 (is-C-keyword? else)
 (is-C-keyword? synchronized)
]

The macro works by defining a @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{literal set} and then a @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{syntax class}.

@racketfile{define-datum-literal-set.rkt}
