#lang racket/base

;; Configuration / back-end tools.
;; - defines the `syntax-parse-examples` raco command
;; - ???

;; =============================================================================

(define (make-new-example macro-name)
  ;; make folder
  ;; make 3 files (src, scrbl, -test
  ;; edit index.scrbl
  (displayln "make new example"))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (define *did-something?* (make-parameter #f))
  (define-syntax-rule (do-something e* ...)
    (begin (*did-something?* #t) e* ...))
  (define (cmdline argv)
    (parse-command-line "syntax-parse-examples" argv
     `((once-any
        [("-n" "--new")
         ,(λ (flag macro-name)
            (do-something (make-new-example macro-name)))
         ("Make a new example" "string?")
         ]))
     (λ ARG*
       (unless (*did-something?*)
         (cmdline '#("--help"))))
     '()))
  (cmdline (current-command-line-arguments)))
