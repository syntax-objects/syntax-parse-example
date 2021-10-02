#lang racket
(provide defines)
(require syntax/parse/define)

(define-syntax-parser defines
  [(_ (~or [id:id expr:expr]
           [(idv:id ...+) expr:expr]) ...+)
   #'(begin
       (~? (define id expr)
           (define-values (idv ...) expr)) ...)])
