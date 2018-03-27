#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse syntax-parse-example/cross-macro-communication/cross-macro-communication)]

@(define cross-macro-communication-eval
   (make-base-eval '(require racket
                             syntax-parse-example/cross-macro-communication/cross-macro-communication)))

@title{Cross Macro Communication}

@; =============================================================================

@defmodule[syntax-parse-example/cross-macro-communication/cross-macro-communication]{}

@deftogether[(
 @defform[(define-for-macros id expr)]
 @defform[(get-macroed id)]
)]{
  The @racket[define-for-macros] and @racket[get-macroed]
  demonstrate the use of @racket[syntax-local-value] when
  communicating information across macros. Anything defined
  with @racket[define-for-macros] can be accessed (at
  compile/macro expansion time) by @racket[get-macroed].

  @examples[#:eval cross-macro-communication-eval
            (define-for-macros cake 42)
            (get-macroed cake)
            (eval:error cake)
  ]

  This communication works even if the identifiers are defined and used in different files or modules:

  @examples[#:eval cross-macro-communication-eval
            (module the-definition racket
              (require syntax-parse-example/cross-macro-communication/cross-macro-communication)
              (define-for-macros shake 54)
              (provide shake))
            (module the-use racket
              (require 'the-definition
                       syntax-parse-example/cross-macro-communication/cross-macro-communication)
              (get-macroed shake))
            (require 'the-use)]

  The following is the source code for @racket[define-for-macros] and @racket[get-macroed]:

  @racketfile{cross-macro-communication.rkt}

  In @racket[define-for-macros], the macro simply binds a new
  value at compile time using @racket[define-syntax]. In this
  example @racket[define-for-macros] is mostly synonymous with
  @racket[define-syntax], but it demonstrates that the
  @racket[name] could be changed (to say add a question mark
  at the end), and the given expression can be changed. The
  @racket[get-macroed] form simply takes the compile time value and
  puts it in the run time module. If @racket[name] is used
  outside of a macro then a syntax error is raised.
}
