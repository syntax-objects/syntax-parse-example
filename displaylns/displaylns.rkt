#lang racket
(provide displayln*)
(require syntax/parse/define)

(define-syntax-parse-rule (displayln* items:expr ...)
  (displayln (string-join (map ~a (list items ...)) " ")))
