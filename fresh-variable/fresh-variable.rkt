#lang racket/base

(provide fresh-variable)

(require racket/syntax syntax/parse)

(define-syntax-class (fresh-variable [context #f])
  #:attributes (fresh-var)
  (pattern name
           #:with temp-var (generate-temporary #'name)
           #:with fresh-var (if context
                                (format-id context "~a" #'temp-var)
                                #'temp-var)))
