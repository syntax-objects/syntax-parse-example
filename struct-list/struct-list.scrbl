#lang syntax-parse-example
@require[
  (for-label typed/racket/base racket/syntax syntax/parse syntax-parse-example/struct-list/struct-list)]

@(define struct-list-eval
   (make-base-eval #:lang 'typed/racket/base '(require syntax-parse-example/struct-list/struct-list)))

@title{@tt{struct-list}}

@; =============================================================================

@defmodule[syntax-parse-example/struct-list/struct-list]{}

@defform[(struct-list expr ...)]{
  The @racket[struct-list] macro has similar syntax as Typed Racket's @racket[struct] form, but creates a new datatype backed by a @racket[list] instead of an actual struct.
  The @racket[#:type-name] keyword is required and must supply a name that is different from the struct name.

  @examples[#:eval struct-list-eval
   (struct-list foo ([a : String] [b : String]) #:type-name Foo)
   (define f (foo "hello" "world"))
   (foo? f)
   (string-append (foo-a f) " " (foo-b f))
   (ann f Foo)
  ]

  The implementation:
  @itemlist[#:style 'ordered
    @item{
      extracts the names and type names from the syntax,
    }
    @item{
      creates an identifier for the predicate and a sequence of identifiers for the accessors (see the @racket[#:with] clauses),
    }
    @item{
      and defines a constructor and predicate and accessor(s).
    }
  ]

  @racketfile{struct-list.rkt}

}
