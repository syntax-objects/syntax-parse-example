#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/fnarg/fnarg)]

@title{Function Parameter Syntax Class}
@stxbee2021["shhyou" 23]

@; =============================================================================

@defmodule[syntax-parse-example/fnarg/fnarg]{}

Syntax classes offer a powerful mechanism for abstracting over classes of
conceptually related patterns. Moreover, syntax classes can be parameterized,
with @racket[expr/c] being one prominent example.

In this example, we define a syntax class that roughly captures the grammar of
formal parameters in function headers. It parses the name of the formal
parameter, the default expressions of optional arguments and the keywords of
keyworded arguments.

@defidform[fnarg]{
  The @racket[fnarg] syntax class matches the basic argument patterns that
  can appear in function headers: mandatory, optional, and keyword arguments.
  The class optionally accepts two parameters to toggle whether optional and/or
  keyword arguments are allowed.

  Refer to the source file @tt{fnarg-test.rkt} for an example use.

  @racketfile{fnarg.rkt}

}
