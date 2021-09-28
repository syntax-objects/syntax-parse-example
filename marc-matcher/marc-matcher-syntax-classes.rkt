#lang racket
(provide marc-var-defn)

(require syntax/parse)

(define-syntax-class marc-var-defn
  #:auto-nested-attributes
  (pattern (re:regexp #:as name:id)))
