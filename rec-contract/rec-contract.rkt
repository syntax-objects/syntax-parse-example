#lang racket/base
(provide rec/c)
(require racket/contract (for-syntax racket/base syntax/parse))

(define-syntax-rule (rec/c t ctc)
  (letrec ([rec-ctc
            (let-syntax ([t (syntax-parser (_:id #'(recursive-contract rec-ctc)))])
              ctc)])
      rec-ctc))
