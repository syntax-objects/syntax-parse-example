#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse
             syntax-parse-example/try-catch-finally/3)]

@(define try-catch-finally-eval-3
   (make-base-eval '(require racket/port racket/string syntax-parse-example/try-catch-finally/3)))


@defmodule[syntax-parse-example/try-catch-finally/3]{}
@stxbee2021["eutro" 12]
@adapted-from[#:what "package" @tt{try-catch-match} "https://docs.racket-lang.org/try-catch-match/index.html"]

@defform[#:literals [catch finally]
         (try body ...+ catch-clause ... maybe-finally-clause)
         #:grammar ([catch-clause (catch pat body ...+)]
                    [maybe-finally-clause (code:line)
                                          (finally body ...+)])]{

  Evaluates the body expressions in a context that matches
  any exceptions against the @racket[catch] clauses in succession.
  Evaluates the @racket[finally] clause when leaving the dynamic
  extent of the try expression's body.

  The expressiveness of match syntax makes it sufficiently flexible for any case,
  and grants familiarity to those that are used to it.

  @examples[#:eval try-catch-finally-eval-3
    (try
      (cons 1)
      (catch (or (? exn? (app exn-message msg))
                 (app ~a msg))
        (car (string-split msg ";")))
      (finally (displayln "finally")))
  ]

}

@defform[(try-with ([id val-expr] ...) expr ...+)]{
  The @racket[try-with] macro (and its cousin @racket[try-with*])
  generalize resource cleanup in an exception-safe way.
  These macros are influenced by @tt{with-open} from Clojure and
  @tt{try-with-resources} from Java.

  Like @racket[let], bind @racket[id]s to @racket[val]s in @racket[body].
  @racket[val]s are evaluated with @racket[current-custodian] set to a
  new custodian created with @racket[make-custodian]. When the expression returns,
  the custodian is shutdown with @racket[custodian-shutdown-all] in a
  @racket[finally] clause.

  @examples[#:eval try-catch-finally-eval-3
    (try-with ([port (open-output-string)])
      (displayln "Hello!" port))
  ]
}

@defform[(try-with* ([id val-expr] ...) expr ...+)]{
  Like @racket[try-with], but the binding of each @racket[id] is made available
  in subsequent @racket[val]s, as in @racket[let*].
  All @racket[val]s are evaluated with the same custodian.
}

@deftogether[(
  @defidform[catch]
  @defidform[finally])]{
  These identifiers may only appear within a @racket[try] form.
}

@racketfile{3.rkt}

