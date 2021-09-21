#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse racket/syntax syntax/datum syntax-parse-example/define-with-datum/define-with-datum)]

@(define define-with-datum-eval
   (make-base-eval '(require syntax/datum syntax-parse-example/define-with-datum/define-with-datum)))

@title{@tt{define/with-datum}}

@; =============================================================================

@defmodule[syntax-parse-example/define-with-datum/define-with-datum]{}
@stxbee2021["shhyou" 21]

@defform[(define/with-datum pattern datum-expr)]{

  Definition form of @racket[with-datum].
  Matches the value result of @racket[datum-expr] and binds the pattern variables
  in @racket[pattern].

  The following example defines three pattern variables:
  @racket[x] gets bound to a string,
  @racket[y] (at @tech/syntax{ellipsis depth} 1) gets bound to a list of strings,
  and @racket[z] (at ellipsis depth 2) gets bound to a list of lists of strings.

  @examples[#:eval define-with-datum-eval
    (define/with-datum (x ((y z ...) ...))
      '("X" (("Y1" "Z11" "Z12")
             ("Y2" "Z21"))))
    (datum x)
    (with-datum ([w "W"])
      (datum ((y w) ...)))
    (datum ((^ z ... $) ...))
  ]

  The implementation is similar to that of @racket[define/with-syntax]
  (@hyperlink["https://github.com/racket/racket/blob/8e83dc25f7f5767d9e975f20982fdbb82f62415a/racket/collects/racket/syntax.rkt#L22-#L59" "link"])
  but uses @racket[syntax-parse] pattern directives to express the procedural
  code from the original.
  These pattern directives allow escaping into arbitrary expansion-time
  computation while retaining appropriate semantical meanings such as binding a
  pattern variable (@racket[#:with]) or performing an imperative action (@racket[#:do]).

  @racketfile{define-with-datum.rkt}

}
