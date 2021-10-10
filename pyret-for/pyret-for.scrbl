#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/pyret-for/pyret-for)]

@(define pyret-for-eval
   (make-base-eval '(require racket/math racket/list racket/string syntax-parse-example/pyret-for/pyret-for)))

@title{@tt{pyret-for}}
@stxbee2021["sorawee" 11]

@; =============================================================================

@defmodule[syntax-parse-example/pyret-for/pyret-for]{}

@defform[(pyret-for fn-expr ([pattern arg] ...) expr ...+)]{
  Many common higher-order functions consume a function value as the first
  argument, and @racket[_n] more arguments after that, where the function value
  accepts @racket[_n] arguments, which corresponds to the arguments in the call
  in some way.
  Examples include: @racket[map], @racket[filter] (only one argument),
  @racket[andmap], @racket[ormap].
  (@racket[foldl] and @racket[foldr] have arguments in a wrong order, so they
  don't quite work.)

  @examples[#:label @elem{Example without @racket[pyret-for]:} #:eval pyret-for-eval
    (define things     '(("pen") ("pineapple") ("apple") ("pen")))
    (define quantities '(1       2             3         5))

    (andmap (λ (thing quantity)
              (or (string-contains? (first thing) "apple")
                  (odd? quantity)))
            things
            quantities)

  ]

  The problem is that:

  @itemlist[
    @item{
      It is difficult for readers to relate formal arguments of the function
      value to the actual arguments of the call.
    }
    @item{
      There is a lot of rightward drift.
    }]

  @examples[#:label @elem{Example with @racket[pyret-for]:} #:eval pyret-for-eval
    (define things     '(("pen") ("pineapple") ("apple") ("pen")))
    (define quantities '(1       2             3         5))

    (pyret-for andmap ([thing things] [quantity quantities])
      (or (string-contains? (first thing) "apple")
          (odd? quantity)))
  ]

  The @racket[pyret-for] syntax, based on @hyperlink["https://www.pyret.org/docs/latest/Expressions.html#%28part._s~3afor-expr%29" @elem{Pyret's @tt{for}}],
  can be used to invoke this kind of higher-order function.

  @racket[pyret-for] additionally improves upon Pyret's for by allowing
  arbitrary match pattern.

  @examples[#:label @elem{Example with @racket[pyret-for] and match:} #:eval pyret-for-eval
    (define things     '(("pen") ("pineapple") ("apple") ("pen")))
    (define quantities '(1       2             3         5))

    (pyret-for andmap ([(list thing) things] [quantity quantities])
      (or (string-contains? thing "apple")
          (odd? quantity)))]

  Implementation:

  @racketfile{pyret-for.rkt}

}
