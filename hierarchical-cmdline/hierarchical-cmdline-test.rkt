#lang racket/base
(module+ test
  (require rackunit racket/cmdline racket/port syntax-parse-example/hierarchical-cmdline/hierarchical-cmdline)

  (test-begin
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

    (test-case "ex1"
      (check-equal?
        (with-output-to-string
          (lambda ()
            (parameterize ([current-command-line-arguments (vector "--move" "--relative" "--left" "--jump" "--jump" "--move" "--absolute" "--south" "--jump")])
              (parse-main))))
        "You're going left!\nYou're jumping!\nYou're jumping!\nYou're going south!\nYou're jumping!\n")))
)
