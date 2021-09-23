#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax/parse/define syntax-parse-example/displaylns/displaylns)]

@(define displaylns-eval
   (make-base-eval '(require syntax-parse-example/displaylns/displaylns) '(define a-port (current-output-port))))

@title{@tt{displaylns}}

@; =============================================================================

@defmodule[syntax-parse-example/displaylns/displaylns]{}
@stxbee2021["Lazerbeak12345" 2]

@defform[(displayln* expr ...)]{
  This macro is intended to make debugging easier by allowing a programmer to
  print a batch of values all in one go much like Python's
  @hyperlink["https://docs.python.org/3/library/functions.html#print" "print"].

  To change the output port use @racket[parameterize].

  @examples[#:eval displaylns-eval
    (displayln* 1 2 3 4 5 '(this is a list of datums 1 2 3 "hi"))
    (parameterize ([current-output-port a-port])
      (displayln* 1 2 '(a b)))
  ]

  With @racket[define-syntax-parse-rule], this macro is a one-liner:

  @racketfile{displaylns.rkt}

  A function could express the same behavior.
  Furthermore, using a function instead would help reduce code size ---
  assuming the compiler does not inline every call to @racket[displayln*].
  That said, a macro has two advantages for this debugging tool:
  @itemize[
  @item{
    You can easily to hide all the debug expressions by changing the macro
    body to @racket[(void)].
  }
  @item{
    You might extend the macro to compute source locations from its arguments.
  }
  ]

}
