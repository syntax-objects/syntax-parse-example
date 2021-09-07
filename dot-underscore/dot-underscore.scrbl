#lang scribble/manual

@require[
  (for-label racket/base #;syntax-parse-example/dot-underscore/dot-underscore)]

@title{@tt{dot-underscore}}

@; =============================================================================

@defmodule[syntax-parse-example/dot-underscore/dot-underscore]{}

The @tt{dot-underscore} example shows how to:
@itemlist[
  @item{implement dot notation for object field access using a custom @racket[#%top]}
  @item{implement method invocation without explicit @racket[send] using a custom @racket[#%app]}]


See the source file @tt{dot-underscore.rkt} for explanation.

There are examples in @tt{test-dot-underscore.rkt}.




