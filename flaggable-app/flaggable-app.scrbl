#lang syntax-parse-example
@require[
  (for-label (rename-in racket/base [#%app racket:#%app]) (except-in racket/base #%app) syntax/parse syntax-parse-example/flaggable-app/flaggable-app)]

@(define plain-eval
   (make-base-eval '(require racket/string)))

@(define flaggable-app-eval
   (make-base-eval '(require racket/string syntax-parse-example/flaggable-app/flaggable-app)))

@title{@tt{flaggable-app}}
@stxbee2021["sorawee" 14]

@; =============================================================================

@defmodule[syntax-parse-example/flaggable-app/flaggable-app]{}

@defform[(#%app fn expr ...+)]{
  Many functions accept optional boolean keyword arguments.
  These arguments are known as flags.
  As a simple example, the following function accepts two flags
  @racket[#:left] and @racket[#:right]:

  @examples[#:label #f #:eval flaggable-app-eval
    (define (trim s #:left? [left? #f] #:right? [right? #f])
      (string-trim s #:left? left? #:right? right?))
  ]
  @examples[#:hidden #:eval plain-eval
    (define (trim s #:left? [left? #f] #:right? [right? #f])
      (string-trim s #:left? left? #:right? right?))
  ]

  The function may be invoked with any number of flags, but if a flag keyword
  appears then it needs an argument as well:

  @examples[#:label #f #:eval plain-eval
    (trim " 1 2 3 " #:left? #t)
    (eval:error (trim " 1 2 3 " #:left?))
  ]

  Flaggable @racket[#%app] allows users to instead write:

  @examples[#:label #f #:eval flaggable-app-eval
    (trim " 1 2 3 " #:left?)
    (trim " 1 2 3 " #:left? #:right?)
  ]

  That is, a keyword that doesn't come with an argument will default the
  value to @racket[#t]. Arguments are still supported.

  This does come at a cost: all keyword arguments must be specified after
  positional arguments to avoid ambiguity. Without this restriction, it is hard
  to tell whether:

  @racketblock[
    (f #:a 1)
  ]

  is meant to be itself or:

  @racketblock[
    (f 1 #:a #t)
  ]

  Note: inspired by @hyperlink["https://www.reddit.com/r/Racket/comments/oytknk/keyword_arguments_without_values/h7w67dd/" "reddit.com/r/Racket/comments/oytknk/keyword_arguments_without_values/h7w67dd"].

  @racketfile{flaggable-app.rkt}

}
