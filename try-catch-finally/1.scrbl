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

@defform[#:literals (catch catch/match finally)
         (try body-expr ...+ maybe-catch maybe-catch/match maybe-finally)
         #:grammar
         [(maybe-catch (code:line)
                       (catch [pred-expr exn-id handler-expr ...+] ...)
                       (catch/match [match-expr handler-expr ...+] ...))
          (maybe-finally (code:line)
                         (finally finally-expr ...+))]]{

  @examples[#:eval try-catch-finally-eval-1
    (try
      (/ 10 0)
      (catch ([(exn? e) (exn-message e)])))

    (let ([resource (open-input-string "")])
      (try
        (write-bytes #"Hi" resource)
        (catch ([(exn? e) (displayln (exn-message e))]))
        (finally
          (close-input-port resource)))
      (port-closed? resource))
  ]

  The @tt{catch} and @tt{finally} parts of the macro are implemented using
  @racket[with-handlers] and @racket[dynamic-wind].

  @racketfile{1.rkt}

}

