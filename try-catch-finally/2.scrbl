#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse
             syntax-parse-example/try-catch-finally/2)]

@(define VUM @hyperlink["https://docs.racket-lang.org/syntax/uniform-meanings.html" @emph{Variants with Uniform Meanings}])
@(define syntax-class @seclink["stxparse-specifying" #:doc '(lib "syntax/scribblings/syntax.scrbl") "syntax class"])

@(define try-catch-finally-eval-2
   (make-base-eval '(require syntax-parse-example/try-catch-finally/2)))

@title{@tt{try} --- another try/catch/finally inspired by Gerbil Scheme}

@defmodule[syntax-parse-example/try-catch-finally/2]{}
@stxbee2021["AlexKnauth" 10]
@adapted-from[#:what "package" @tt{try-catch-finally} "https://docs.racket-lang.org/try-catch-finally/index.html"]

@defform[#:literals (catch => finally _)
         (try expr ...+ maybe-catch maybe-finally)
         #:grammar
         [(maybe-catch (code:line)
                       (catch pred => (lambda (x) expr ...+))
                       (catch (pred id) expr ...+)
                       (catch id expr ...+)
                       (catch _ expr ...+))
          (maybe-finally (code:line)
                         (finally expr ...+))]
]{
  A try/catch/finally macro inspired by Gerbil Scheme's @litchar{try} macro in
  @litchar{:std/sugar}.

  @examples[#:eval try-catch-finally-eval-2
    (try
      (raise-syntax-error #f "a syntax error")
      (catch (exn:fail:syntax? e)
        (displayln "got a syntax error")))
    (let/cc up
      (try
        (displayln "at before")
        (up (void))
        (displayln "at after")
        (finally (displayln "out"))))]

  Compare the implementation below to the procedural @racket[syntax-case] baseline
  from Gerbil scheme:
  the @hyperlink["https://github.com/vyzo/gerbil/blob/fa9537be0848e54d2c68165503b9cc48babb9334/src/std/sugar.ss#L32-L97" "try macro"]
  and @hyperlink["https://github.com/vyzo/gerbil/blob/17fbcb95a8302c0de3f88380be1a3eb6fe891b95/src/gerbil/runtime/gx-gambc0.scm#L1625-L1636" "helper functions"].

  @racketfile{2.rkt}

  Overall, this Racket version uses half as much macro code and 70% fewer lines
  of helper code (down from 11 lines to 3). The notes below provide a detailed
  comparison.

  @itemlist[
    @item{
      @emph{Changes in supporting runtime function definitions}
      @itemlist[
        @item{
          The @racket[try] macro does not need a supporting @tt{with-catch} function
          because it can use a combination of the @racket[syntax/parse] technique
          @|VUM| and the existing Racket @racket[with-handlers] form.
        }
        @item{
          Gerbil's supporting function @tt{with-unwind-protect} uses mutable state
          to make a one-time-use closure dynamically check that it really is only called
          once. In the Racket version, @racket[_call-with-try-finally] uses a
          @tech/reference{continuation barrier} (as suggested by SamPh on
          Discord), to accomplish the same without mutable state.
        }]}
    @item{
      @emph{Changes in supporting runtime function definitions}
      @itemlist[
        @item{
          The @racket[catch] and @racket[finally] literals use a compile-time
          helper function @racket[_not-allowed-as-an-expression] to generate
          precise error messages. Gerbil's simply uses empty
          @tt{defrule}s and gives a generic @tt{Bad syntax} error message.
        }
        @item{
          The @racket[try] macro uses @racket[syntax-parse]'s @racket[...+] to
          express one-or-more repetition.
          Gerbil's version uses manual @tt{null?} checks and
          @tt{stx-null?} checks; see the @tt{generate-thunk} helper function and
          the @tt{finally} case.
        }
        @item{
          The @racket[try] macro uses a @|syntax-class| to
          recognize body expressions that are not catch or finally clauses
          via the @racket[~not] pattern.
          Gerbil's version uses a named @tt{let} loop with 2 extra nested
          @tt{syntax-case} expressions to identify body expressions.
        }
        @item{
          The @racket[try] macro uses a @|syntax-class| to
          handling catch clauses as @|VUM| allowing repetition with ellipses.
          Gerbil's version uses the helper
          function @tt{generate-catch} (which hasits own @tt{with-syntax},
          named @tt{let} loop, @tt{match}, and @tt{syntax-case} expressions)
          in combination with another named @tt{let} loop and another nested
          @tt{syntax-case} expression (beyond the ones mentioned above) in
          the main body of the macro to separate the catch clauses from the
          finally clause.
        }
        @item{
          The @racket[try] macro uses a @|syntax-class|
          to handle the @racket[finally] clause.
          Syntax classes allow @racket[try] macro to express this simply by
          putting the finally-clause pattern at the end of the main
          syntax pattern, after the previous patterns and their ellipses.
          Gerbil's version uses a helper
          function @tt{generate-fini} and 2 different @tt{finally} cases in different
          @tt{syntax-case} expressions in the main body of the macro. One of these
          is to separate body expressions from @tt{finally} in the case when there
          are no catches in between, and the other is to separate @tt{catch} clauses
          from @tt{finally}. Their first @tt{finally} case requires a manual @tt{stx-null?}
          check to make sure nothing comes after @tt{finally}, while their
          second @tt{finally} case encodes that into a @tt{syntax-case} pattern
          for a 1-element list.
        }]}
  ]
}

@deftogether[(
  @defidform[catch]
  @defidform[=>]
  @defidform[_]
  @defidform[finally])]{
  These identifiers may only appear within a @racket[try] form.
}

