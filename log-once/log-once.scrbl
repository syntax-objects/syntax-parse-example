#lang syntax-parse-example
@require[
  (for-label racket/base (only-in racket/math natural?) syntax/parse syntax-parse-example/log-once/log-once)]

@(define log-once-eval
   (make-base-eval '(require syntax-parse-example/log-once/log-once)))

@title{@tt{log-once}}

@; =============================================================================

@defmodule[syntax-parse-example/log-once/log-once]{}
@stxbee2021["Fictitious-Rotor" 3]

@defproc[(log-once [#:skip-count skip natural? 0]
                   [#:log-count log-count natural? 1]
                   [#:when cond any/c (void)]
                   [#:message msg string? ""]
                   [#:newline newline? boolean? #f]
                   [expr any/c] ...) void?]{
  The purpose of this macro is to print a sample of values within tight loops---rather
  than inundate the console with thousands of lines of irrelevant data.
  It achieves this purpose by providing a variety of tools that can be used to
  constrain what is logged down to what actually interests the observer.

  @examples[#:eval log-once-eval
    (for ([char (in-string "abcdefghijklmnopqrstuvwxyz")])
      (log-once #:skip-count 18
                #:log-count 3
                char))

    (for ([char (in-string "abcdefGhijkLmnopQrstuVwxyz")])
      (log-once #:skip-count 2
                #:when (char-upper-case? char)
                char))
  ]

  The macro replaces patterns of code that would look something like this:

  @racketblock[
    (code:comment "You have to define the variables somewhere where they won't")
    (code:comment " fall out of scope so that the mutations matter")
    (code:comment "You also have to be wary of leaving any of this code lying")
    (code:comment " around once you're finished with debugging")
    (define is-logged #f)
    (define skip-count 0)
    (for ([char (in-string "abcdefGhijkLmnopQrstuVwxyz")])
      (when (and (not is-logged)
                 (char-upper-case? char)
                 (begin
                   (set! skip-count (add1 skip-count))
                   (> skip-count 2)))
        (printf "char = ~s\n" char)
        (set! is-logged #t)))
  ]

  The set of macros have gone through many revisions (@hyperlink["https://github.com/syntax-objects/Summer2021/issues/3" "link"]).
  This iteration makes use of @racket[~?], @racket[~@],
  @racket[syntax-local-lift-expression], and the excellent @racket[...].

  @racketfile{log-once.rkt}

}
