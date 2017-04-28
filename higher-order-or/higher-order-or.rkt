#lang racket/base
(provide higher-order-or)
(require (for-syntax racket/base syntax/parse))

(define-syntax (higher-order-or stx)
  (syntax-parse stx
   [(_)
    #'#false]
   [(_ a . b)
    #'(let ([a-val a])
        (if a-val a-val (higher-order-or . b)))]
   [_:id
    #'(lambda arg*
        (let loop ([arg* arg*])
          (cond
           [(null? arg*)
            #false]
           [(car arg*)
            (car arg*)]
           [else
            (loop (cdr arg*))])))]))
