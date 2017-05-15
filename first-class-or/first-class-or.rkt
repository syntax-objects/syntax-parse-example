#lang racket/base
(provide first-class-or)
(require (for-syntax racket/base syntax/parse))

(define-syntax (first-class-or stx)
  (syntax-parse stx
   [(_)
    #'#false]
   [(_ a . b)
    #'(let ([a-val a])
        (if a-val a-val (first-class-or . b)))]
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
