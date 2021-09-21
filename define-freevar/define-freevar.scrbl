#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/define-freevar/define-freevar)]

@(define define-freevar-eval
   (make-base-eval '(require syntax-parse-example/define-freevar/define-freevar)))

@title{Functions with Dynamically-Scoped Parameters}
@stxbee2021["shhyou" 24]

@; =============================================================================

@defmodule[syntax-parse-example/define-freevar/define-freevar]{}

@defform[(define/freevar (function-id arg-id ...)
  #:freevars (freevar1-id freevar2-id ...)
  body1-expr body2-expr ...)
]{
  The @racket[define/freevar] macro introduces function
  definitions with free variables in their body.
  The free variables are resolved non-hygienically to any
  bindings of an equal symbol name at each use site.

  One motivating example is the following helper function for Redex models:

  @examples[#:label #f #:eval define-freevar-eval
    (require racket/pretty redex/reduction-semantics)
    (define/freevar (apply-reduction-relation*--> term)
      #:freevars (-->R)
      (pretty-print term)
      (for/fold ([term-list (list (list #f term))])
                ([step (in-naturals)]
                 #:break (null? term-list))
        (define new-terms
          (apply-reduction-relation/tag-with-names -->R (list-ref (car term-list) 1)))
        (pretty-print new-terms)
        new-terms))
  ]

  This second example dynamically looks for a value @racket[_who] to format an
  error message:

  @examples[#:label #f #:eval define-freevar-eval
    (define/freevar (raise-who-error message source-stx)
      #:freevars (who)
      (raise-syntax-error who
                          message
                          source-stx))
    (eval:error
      (let ([who 'knock-knock])
        (raise-who-error "who's there" #'door)))
  ]

  Conceptually, the @racket[define/freevar] form expands into a new definition
  having the original code and a new macro that generates references for
  the free variables:

  @racketblock[
    (define (raise-who-error/impl who message source-stx)
      (raise-syntax-error who
                          message
                          source-stx))

    (define-syntax (raise-who-error stx)
      (syntax-parse stx
        [(proc-src:id args ...)
         #:with who/use-site (syntax-property
                              (format-id stx "~a" 'who #:source #'proc-src)
                              'original-for-check-syntax #t)
         (syntax/loc stx
           (raise-who-error/impl who/use-site args ...))]))
  ]

  The new macro @racket[_raise-who-error] creates a reference, @racket[_who/use-site],
  to be captured non-hygienically using the context from the use site.
  The expansion then proceeds with the use-site reference and calls
  the original code.

  Additionally, the use-site references have the source location of the
  @racket[_proc-src] identifier and the syntax property @racket['original-for-check-syntax]
  so that Check Syntax and DrRacket can draw the binding arrows.

  Caveat: mutation on the free variables will not reflect on the original
  binding. This restriction can be overcome using @racket[make-set!-transformer].

}

@defform[(with-freevar function-id ([freevar-id new-freevar-id] ...)
  body-expr1 body-expr2 ...) ]{
  Locally renames the free variables for @racket[define/freevar] definitions.
}
@defform[(define/with-freevar new-function-id old-function-id
  [freevar-id new-freevar-id]
  ...)]{
  Definition form of @racket[with-freevar].
}

@section{Implementation}

While the idea is straightforward, a direct translation generates a large
amount of code duplication. In the output of @racket[define/freevar], the only
varying parts are the names of the free variables and the identifier
of the actual implementation. The implementation of @racket[define/freevar]
thus follows the common pattern of using a struct to share the transformer code.

@itemlist[#:style 'ordered
  @item{
    The @racket[define/freevar] form expands to a new definition storing the
    original code and a macro for binding the free identifiers.
  }
  @item{
    The implementation introduces a procedure-like struct, @racket[_open-term],
    that holds the list of free variables and the identifier of
    the actual code.
  }
  @item{
    When the macro expander applies an instance of @racket[_open-term], it
    extracts names of the free variables and redirects the reference to the
    actual code.
  }
]

The idea behind custom @tech/syntax{pattern expanders} and syntax class aliases
(see @racket[prop:syntax-class]) are related:
using structs to store varying information while attaching struct type
properties to assign behavior.

  @racketfile{define-freevar.rkt}

