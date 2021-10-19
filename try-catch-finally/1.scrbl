#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse
             syntax-parse-example/try-catch-finally/1)]


@(define try-catch-finally-eval-1
   (make-base-eval '(require syntax-parse-example/try-catch-finally/1)))

@title{@tt{try} --- a try/catch/finally for sarna}

@defmodule[syntax-parse-example/try-catch-finally/1]{}
@stxbee2021["benknoble" 9]
@adapted-from[#:what "package" @tt{try-make-sarna-happy} "https://docs.racket-lang.org/try-make-sarna-happy/index.html"]

@defform[(try body-expr ...+ maybe-catch maybe-catch/match maybe-finally)
         #:grammar
         [(maybe-catch (code:line)
                       (catch [pred-expr exn-id handler-expr ...+] ...)
                       (catch/match [match-expr handler-expr ...+] ...))
          (maybe-finally (code:line)
                         (finally finally-expr ...+))]]{

  Tries @racket[body-expr]s in order, returning the value of the last. If an
  exception is raised, it may be handled by one of the @racket[catch] clauses,
  whose value becomes the value of the overall form. The optional
  @racket[finally] clause is @emph{always} run.

  The @racket[catch] clauses use @racket[with-handlers], but in a different
  format: when @racket[pred-expr] returns true for a thrown exception,
  @racket[exn-id] is bound to the exception for the body @racket[handler-expr].

  The @racket[catch/match] clauses are @racket[match] forms tested against the
  exception.

  When both @racket[catch]-style and @racket[catch/match]-style clauses are
  present, all of the @racket[catch]-style clauses are tried before any of the
  @racket[catch/match] clauses.

  @examples[#:eval try-catch-finally-eval-1
    (try
      (/ 10 0)
      (catch [exn:fail? e (exn-message e)]))

    (let ([resource (open-input-string "")])
      (try
        (write-bytes #"Hi" resource)
        (catch [exn? e (displayln (exn-message e))])
        (finally
          (close-input-port resource)))
      (port-closed? resource))

    (struct posn [x y])
    (try (raise (posn 1 2))
         (catch [exn? e (exn-message e)])
         (catch/match [(posn 1 y) y]))
  ]

}

@defform[(catch [pred-expr exn-id handler-expr ...+])]{
Used in @racket[try] to specify exception handlers. An exception is tested
against each @racket[pred-expr]; if the test succeeds, the exception is bound to
@racket[exn-id] and @racket[handler-expr] runs.

If no @racket[pred-expr]s succeed, testing proceeds to any @racket[catch/match]
clauses.
}

@defform[(catch/match [match-expr handler-expr ...+])]{
Used in @racket[try] to specify exception handlers. An exception is matched
(in the sense of @racket[match]) against @racket[match-expr]; if the match
succeeds @racket[handler-expr] runs.

If no @racket[match-expr]s succeed, the exception is re-@racket[raise]d.
}

@defform[(finally finally-expr ...+)]{
Used in @racket[try] to specify a body of @racket[finally-expr]s which run when
the @racket[try]-body exits, be it through a return, continuation jump, or
exception.
}


Implementation:

@racketfile{1.rkt}

