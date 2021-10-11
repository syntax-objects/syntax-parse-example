#lang racket/base
(provide shift-command-line-arguments parameterize-help-if-empty-ccla)
(require syntax/parse/define racket/vector)

;; Remove the first argument of the command line arguments
(define-syntax-parse-rule (shift-command-line-arguments body ...)
  (λ args
    (parameterize ([current-command-line-arguments (vector-copy (current-command-line-arguments) 1)])
      body ...)))

;; If the command line arguments are empty, re-parameterize it to
;; default to #("--help")
(define-syntax-parse-rule (parameterize-help-if-empty-ccla body ...)
  (let ([ccla (current-command-line-arguments)])
    (parameterize ([current-command-line-arguments
                    (if (vector-empty? ccla)
                      #("--help")
                      ccla)])
      body ...)))

