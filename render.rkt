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

  racketfile
  ;; Usage: @racketfile{filename}
  ;;  where `filename` is a string representing a Racket file
  ;; Renders the contents of `filename` as if they were copy/pasted into
  ;;  a `racketblock`
)

(require
  scribble/doclang
  scribble/example
  scribble/manual
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

(begin-for-syntax
  (define-runtime-path here "."))

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
