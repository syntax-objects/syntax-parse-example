#lang syntax-parse-example
@require[
  (for-label racket/base racket/cmdline syntax/parse syntax-parse-example/hierarchical-cmdline/hierarchical-cmdline)]

@(define hierarchical-cmdline-eval
   (make-base-eval '(require racket/cmdline syntax-parse-example/hierarchical-cmdline/hierarchical-cmdline)))

@title{Hierarchical parsing of command-line arguments}
@stxbee2021["Metaxal" 16]
@nested[#:style 'inset @emph{Adapted from a @hyperlink["https://github.com/jackfirth/resyntax/pull/147/files" @elem{PR to @tt{resyntax}}]}]

@; =============================================================================

@defmodule[syntax-parse-example/hierarchical-cmdline/hierarchical-cmdline]{}

@defform[(shift-command-line-arguments body ...)]{
}

@defform[(parameterize-help-if-empty-ccla body ...)]{
}

The purpose of the first macro is to make it easy to parse command line
arguments in a hierarchical way using the built-in @racket[command-line] form. The
second macro is an additional helper that displays the help message
automatically when no command-line argument is specified at this level, which
avoids the case where the user tries one argument is then has no information
about what to do next.

@examples[#:eval hierarchical-cmdline-eval
  (define prog "my-prog")

  (define (parse-relative)
    (parameterize-help-if-empty-ccla
     (command-line
      #:program (string-append prog " --relative")
      #:once-each
      [("--left") => (shift-command-line-arguments
                      (displayln "You're going left!")
                      (parse-main))
                  '("Go to the left")]
      [("--right") => (shift-command-line-arguments
                      (displayln "You're going right!")
                      (parse-main))
                  '("Go to the right")])))

  (define (parse-absolute)
    (parameterize-help-if-empty-ccla
     (command-line
      #:program (string-append prog " --absolute")
      #:once-each
      [("--north") => (shift-command-line-arguments
                       (displayln "You're going north!")
                       (parse-main))
                   '("Go to the north")]
      [("--south") => (shift-command-line-arguments
                       (displayln "You're going south!")
                       (parse-main))
                   '("Go to the south")])))

  (define (parse-move)
    (parameterize-help-if-empty-ccla
     (command-line
      #:program (string-append prog " --move")
      #:once-each
      [("--relative") => (shift-command-line-arguments (parse-relative))
                      '("Specify a relative direction")]
      [("--absolute") => (shift-command-line-arguments (parse-absolute))
                      '("Specify an absolute direction")])))

  (define (parse-main)
    (command-line
     #:program prog
     #:once-each
     [("--move") => (shift-command-line-arguments (parse-move))
                 '("Specify directions")]
     [("--jump") => (shift-command-line-arguments
                     (displayln "You're jumping!")
                     (parse-main))
                 '("jump")]))

  (code:comment "$ racket syntax-bee.rkt --move --relative --left --jump --jump --move --absolute --south --jump")
  (parameterize ([current-command-line-arguments (vector "--move" "--relative" "--left" "--jump" "--jump" "--move" "--absolute" "--south" "--jump")])
    (parse-main))
]

Implementation:

@racketfile{hierarchical-cmdline.rkt}

