#lang syntax-parse-example
@require[
  (for-label racket/base racket/contract syntax/parse syntax-parse-example/syntax-class-contract/syntax-class-contract
             syntax/parse/experimental/reflect)]

@(define syntax-class-contract-eval
   (make-base-eval
     '(require
        (for-syntax
          racket/base
          racket/contract
          (only-in syntax/parse str)
          (only-in syntax/parse/experimental/reflect reify-syntax-class)
          syntax-parse-example/syntax-class-contract/syntax-class-contract))))

@title{@tt{syntax-class-contract}}

@; =============================================================================

@defmodule[syntax-parse-example/syntax-class-contract/syntax-class-contract]{}

@defproc[(syntax-class-contract [cls reified-syntax-class?]) flat-contract?]{
  The @racket[syntax-class-contract] function is one way to put a contract on
   a macro.
  If you give it a syntax class value, it returns a contract that accepts a syntax
   object @racket[#'(_A _B)] where @racket[_A] is anything and @racket[_B]
   matches the syntax class.
  The contract can be attached to a normal macro through the @racket[contract] form.

  @examples[#:eval syntax-class-contract-eval
    (define-syntax add-hello
      (contract
        (-> (syntax-class-contract (reify-syntax-class str))
            syntax?)
        (lambda (stx)
          (let ([orig-str (syntax-e (cadr (syntax-e stx)))])
            (with-syntax ([new-str (string-append "hello" " " orig-str)])
              #'new-str)))
        'this-macro
        'the-macro-user))
    (add-hello "world")
    (eval:error (add-hello 'not-string))
  ]

  Implementation:

  @racketfile{syntax-class-contract.rkt}

  Special thanks to Michael Ballantyne for developing this macro tool.

  Challenge: try making a kind of @racket[contract-out] that can attach
   contracts to macros.
}

@;#lang racket
@;(require (for-syntax racket/contract syntax/parse syntax/parse/experimental/reflect)
@;         (for-meta 2 racket/base syntax/parse ))
@;(provide m)
@;; Option 1, allowing an arbitrary pattern:
@;(begin-for-syntax
@;  (define-syntax syntax-parse/c
@;    (syntax-parser
@;      [(_ p)
@;       #'(flat-named-contract '(syntax-parse/c p)
@;                              (syntax-parser [p #t] [_ #f]))])))
@;(define-syntax m
@;  (contract
@;   (-> (syntax-parse/c (_ e:string))
@;       syntax?)
@;   (lambda (stx) #'5)
@;   'foo
@;   'bar))
@;; Option 2, using a reified syntax class
@;(begin-for-syntax
@;  (define (syntax-parse-arg/c cls)
@;    (flat-named-contract `(syntax-parse-arg/c ,cls)
@;                         (syntax-parser [(_ (~reflect arg (cls))) #t] [_ #f]))))
@;(define-syntax m2
@;  (contract
@;   (-> (syntax-parse-arg/c (reify-syntax-class string))
@;       syntax?)
@;   (lambda (stx) #'5)
@;   'foo
@;   'bar))
