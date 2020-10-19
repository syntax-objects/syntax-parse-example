#lang racket/base
(provide syntax-class-contract)
(require racket/contract
         syntax/parse
         syntax/parse/experimental/reflect)

(define (syntax-class-contract cls)
  (flat-named-contract `(syntax-parse-arg/c ,cls)
    (syntax-parser
      [(_ (~reflect arg (cls))) #true]
      [_ #false])))

