#lang racket/base

;; Helper functions for documenting syntax-parse examples

(provide
  (except-out
    (all-from-out scribble/doclang scribble/example scribble/manual) #%module-begin)
  (for-syntax
    (all-from-out racket/base syntax/parse))

  (rename-out [module-begin #%module-begin])

  tech/guide
  ;; Usage: @tech/guide{text}
  ;;  where `text` refers to a technical definition.
  ;; Short for @tech[#:doc ....]{text},
  ;;  where the `....` is the module path for The Racket Guide

  tech/reference
  ;; Usage: @tech/reference{text}
  ;;  where `text` refers to a technical definition.
  ;; Short for @tech[#:doc ....]{text},
  ;;  where the `....` is the module path for The Racket Reference
  ;; (If the name is too long for you, `rename-in` to something shorter.)

  tech/syntax
  ;; Usage: @tech/syntax{text}
  ;;  where `text` refers to a definition from the `syntax` lib.

  racketfile
  ;; Usage: @racketfile{filename}
  ;;  where `filename` is a string representing a Racket file
  ;; Renders the contents of `filename` as if they were copy/pasted into
  ;;  a `racketblock`

  stxbee2021
  ;; Usage @stxbee2021[user issue]
  ;; Renders a thank-you note for a Syntax Bee 2021 submission

  adapted-from
  ;; Usage @adapted-from[#:what [kind #f] name url]

  stxbee2021-issue
)

(require
  scribble/doclang
  scribble/example
  scribble/manual
  (only-in racket/format ~a)
  (for-syntax
    racket/runtime-path
    racket/path
    racket/base
    syntax/location
    syntax/parse
    (only-in racket/file file->list)))

;; =============================================================================

(define-syntax (module-begin stx)
  (syntax-parse stx
   [(_ id . body)
    (syntax/loc stx
      (#%module-begin id values () . body))]))

(define-syntax (racketfile stx)
  (syntax-parse stx
   [(_ file-name:str)
    #:with (str* ...)
           (file->list (let* ([fn (syntax-e #'file-name)]
                              [dir (syntax-source-directory stx)])
                         (cond
                          [(complete-path? fn)
                           fn]
                          [dir
                           (build-path dir fn)]
                          [else
                           (raise-argument-error 'racketfile "cannot find source for '~a'" fn)]))
                       (lambda (p)
                         (let ([v (read-line p)])
                           (if (eof-object? v) v (string-append v "\n")))))
    (with-syntax ((ctx (syntax/loc stx #'file-name)))
      (syntax/loc stx
        (typeset-code
          #:context ctx
          (quote str*) ...)))]))

(define (tech/guide . text)
  (keyword-apply tech '(#:doc) '((lib "scribblings/guide/guide.scrbl")) text))

(define (tech/reference . text)
  (keyword-apply tech '(#:doc) '((lib "scribblings/reference/reference.scrbl")) text))

(define (tech/syntax . text)
  (keyword-apply tech '(#:doc) '((lib "syntax/scribblings/syntax.scrbl")) text))

(define (github-user usr)
  (hyperlink (format "https://github.com/~a" usr) (tt usr)))

(define (github-issue owner repo item)
  (hyperlink (format "https://github.com/~a/~a/issues/~a" owner repo item) (tt "#" item)))

(define (stxbee2021-issue item)
  (github-issue "syntax-objects" "Summer2021" item))

(define (nested-inset content)
  (nested #:style 'inset content))

(define (stxbee2021 user issue)
  (nested-inset
    (emph "Contributed by "
          (github-user user)
          " ("
          (stxbee2021-issue (~a issue))
          ") during the 2021 Syntax Parse Bee."
          )))

(define (adapted-from #:what [what-type #f] name url)
  (define-values [what-pre what-post]
    (if what-type
      (values " the " (string-append " " what-type))
      (values " " "")))
  (nested-inset
    (emph "Adapted from"
          what-pre
          (hyperlink url name)
          what-post ".")))

