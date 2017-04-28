#lang scribble/manual
@require[
  scribble/example
  racket/file
  (for-label racket/base syntax/parse syntax-parse-examples/higher-order-or/higher-order-or)]

@(define higher-order-or-eval
   (make-base-eval
     '(require syntax-parse-examples/higher-order-or/higher-order-or)))

@(require (for-syntax racket/base syntax/parse))
@(define-syntax (racketfile stx)
   (syntax-parse stx
    [(_ name:str)
     #:with str* (with-input-from-file (syntax-e #'name) (lambda () (for/list ((ln (in-lines))) (string-append ln "\n"))))
     (syntax/loc stx
       (racketblock . str*))]))

@title{@tt{higher-order-or}}
@; TODO this is the right abstraction,
@;  just needs an implementation
@racketfile{higher-order-or/higher-order-or.rkt}

@; TODO this gets the code on the page, functionally,
@;  but doesn't do highlighting or clicking
@; typeset-code*[@(map (lambda (s) (string-append s "\n")) @file->lines{higher-order-or/higher-order-or.rkt})]

@;Racket's @racket[or] is a macro, not a function.
@;@margin-note{See also: @secref["and+or" #:doc '(lib "scribblings/guide/guide.scrbl")]}
@;
@;@examples[#:eval (make-base-eval)
@;  (or #false #true)
@;  (eval:error (apply or '(#false #true)))
@;]
@;
@;
@;
@;Racket's built-in @racket[or] macro 
@;
@;@examples[#:eval higher-order-or-eval
@;  (+ 2 2)
@;  (higher-order-or 2 3)
@;  (map higher-order-or '(9 #false 3) '(8 #false #false))
@;]
@;
@;  (or (+ 2 3) (let loop () (loop)))
@;
@;See @racketmodname[syntax-parse-examples/higher-order-or/higher-order-or-test]
@; for more examples.
