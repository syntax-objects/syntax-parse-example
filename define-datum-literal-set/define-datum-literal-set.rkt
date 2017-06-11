#lang racket/base
(provide define-datum-literal-set)
(require (for-syntax racket/base racket/syntax syntax/parse))

(define-syntax (define-datum-literal-set stx)
  (syntax-parse stx
   [(_ cls-name:id (lit*:id ...))
    #:with set-name (format-id stx "~a-set" (syntax-e #'cls-name))
    #'(begin-for-syntax
      (define-literal-set set-name
        #:datum-literals (lit* ...)
        ())
      (define-syntax-class cls-name
        #:literal-sets ([set-name])
        (pattern (~or lit* ...)))) ]))
