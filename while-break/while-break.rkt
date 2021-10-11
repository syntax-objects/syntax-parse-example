#lang racket/base
(provide while break)
(require syntax/parse/define racket/stxparam (for-syntax racket/base))

(define-syntax-parameter break
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside `while`")))

(define-syntax-parse-rule (while condition body ...)
  (call/ec
   (λ (return)
     (syntax-parameterize ([break (make-rename-transformer #'return)])
       (let loop ()
         (when condition
           (begin body ...
                  (loop))))))))

