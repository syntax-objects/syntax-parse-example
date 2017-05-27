#lang racket/base
(provide let-star)
(require (for-syntax racket/base syntax/parse))

(define-syntax (let-star stx)
  (syntax-parse stx
   [(_ ([x:id v:expr]) body* ...+)
    #'(let ([x v])
        body* ...)]
   [(_ ([x:id v:expr] . bind*) body* ...+)
    #'(let ([x v])
        (let-star bind* body* ...))]
   [(_ bad-binding body* ...+)
    (raise-syntax-error 'let-star
      "not a sequence of identifier--expression pairs" stx #'bad-binding)]
   [(_ (bind*))
    (raise-syntax-error 'let-star
      "missing body" stx)]))
