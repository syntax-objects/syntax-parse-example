#lang syntax-parse-example

@require[
  (for-label racket/base )]

@title{@tt{dot-underscore}}
@stxbee2021["soegaard" 15]

@; =============================================================================

@defmodule[syntax-parse-example/dot-underscore/dot-underscore]{}

The @tt{dot-underscore} example shows how to:
@itemlist[
  @item{implement dot notation for object field access using a custom @racket[#%top]}
  @item{implement method invocation without explicit @racket[send] using a custom @racket[#%app]}]


See the source file @tt{dot-underscore.rkt} for explanation.

There are examples in @tt{test-dot-underscore.rkt}.




