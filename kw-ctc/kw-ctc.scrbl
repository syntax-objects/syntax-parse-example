#lang syntax-parse-example
@require[
  (for-label racket/base racket/contract racket/math syntax/parse syntax-parse-example/kw-ctc/kw-ctc)]

@(define kw-ctc-eval
   (make-base-eval '(require racket/contract racket/math syntax-parse-example/kw-ctc/kw-ctc)))

@title{Generate Contracts for Keyword Functions}
@stxbee2021["dstorrs" 19]
@nested[#:style 'inset @emph{Adapted from the
  @hyperlink["https://docs.racket-lang.org/struct-plus-plus/index.html" @tt{struct-plus-plus}]
  module, which contains many other interesting macros (@stxbee2021-issue{18}).}]

@; =============================================================================

@defmodule[syntax-parse-example/kw-ctc/kw-ctc]{}

@defform[(kw-ctc (dom-spec ...) cod-spec)
         #:grammar ([dom-spec [id]
                              [id ctc-expr]
                              [(id default)]
                              [(id default) ctc-expr]]
                    [cod-spec ctc-expr])]{
  Shorthand to write contracts for functions that expect only keyword arguments.

  @examples[#:eval kw-ctc-eval
    (struct pumpkin [name weight color])
    (define/contract (make-pumpkin #:name name #:weight weight #:color [color "Dark Orange"])
      (kw-ctc ([name] [weight natural?] [(color _) string?]) pumpkin?)
      (pumpkin name weight color))
    (make-pumpkin #:name 'roger #:weight 140)
    (make-pumpkin #:name #false #:weight 117 #:color "Indigo")
    (eval:error (make-pumpkin #:weight 999))
  ]

  Implementation:

  @racketfile{kw-ctc.rkt}

}

