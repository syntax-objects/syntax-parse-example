#lang racket/base

;; Configuration / back-end tools.
;; - defines the `syntax-parse-example` raco command
;; - ???

;; For now the philosophy is "keep things straight-line, avoid making functions
;;  and new files"

(require
  racket/runtime-path)

(define-runtime-path SPE-HOME ".")

(define SPE 'syntax-parse-example)

;; =============================================================================

(define (make-new-example! macro-name)
  (define macro-dir (make-example-directory! macro-name))
  (make-example-src! macro-dir macro-name)
  (make-example-doc! macro-dir macro-name)
  (make-example-test! macro-dir macro-name)
  (add-to-index! macro-name)
  (void))

;; make-example-directory! : string? -> path-string?
;; Create a directory to hold a new example.
;; Raise exception if a directory with the given name already exists.
(define (make-example-directory! macro-name)
  (define macro-dir (build-path SPE-HOME macro-name))
  (if (directory-exists? macro-dir)
    (raise-user-error SPE "example '~a' already exists" macro-name)
    (begin (make-directory macro-dir)
           macro-dir)))

(define (displayln* out-file . str*)
  (with-output-to-file out-file #:exists 'replace
    (λ () (for-each displayln str*))))

(define (make-example-src! dir name)
  (displayln* (path-replace-extension (build-path dir name) #".rkt")
    "#lang racket/base"
    (format "(provide ~a)" name)
    "(require (for-syntax racket/base syntax/parse))"
    ""
    (format "(define-syntax (~a stx)" name)
    (format "  (syntax-parse stx")
    (format "   [x #'(void)])) ;; TODO fill in here")))


(define (make-example-doc! dir name)
  (define mod-name (format "syntax-parse-example/~a/~a" name name))
  (displayln* (path-replace-extension (build-path dir name) #".scrbl")
    "#lang syntax-parse-example"
    "@require["
    (format "  (for-label racket/base syntax/parse ~a)]" mod-name)
    ""
    (format "@(define ~a-eval" name)
    (format "   (make-base-eval '(require ~a)))" mod-name)
    ""
    (format "@title{@tt{~a}}" name)
    ""
    "@; ============================================================================="
    ""
    (format "@defmodule[~a]{}" mod-name)
    ""
    (format "@defform[(~a expr ...)]{" name)
    "  @; TODO add intro text here"
    ""
    (format "  @examples[#:eval ~a-eval" name)
    "   @; TODO add examples here"
    "  ]"
    ""
    (format "  @racketfile{~a.rkt}" name)
    ""
    "  @; TODO add description here"
    "}"
    ))

(define (make-example-test! dir name)
  (displayln* (path-replace-extension (build-path dir (string-append name "-test")) #".rkt")
    "#lang racket/base"
    "(module+ test"
    (format "  (require rackunit syntax-parse-example/~a/~a)" name name)
    ""
    "#;(TODO add tests here)"
    ")"))

(define (add-to-index! name)
  (with-output-to-file (build-path SPE-HOME "index.scrbl") #:exists 'append
    (λ ()
      (printf "@include-example{~a}~n" name))))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (define *did-something?* (make-parameter #f))
  (define-syntax-rule (do-something e* ...)
    (begin (*did-something?* #t) e* ...))
  (define (cmdline argv)
    ;; Using `parse-command-line` instead of the prettier `command-line`
    ;;  so this function can call itself recursively
    (parse-command-line (symbol->string SPE) argv
     `((once-any
        [("-n" "--new")
         ,(λ (flag macro-name)
            (do-something (make-new-example! macro-name)))
         ("Make a new example" "string?")
         ]))
     (λ ARG*
       (unless (*did-something?*)
         (cmdline '#("--help"))))
     '()))
  (cmdline (current-command-line-arguments)))
