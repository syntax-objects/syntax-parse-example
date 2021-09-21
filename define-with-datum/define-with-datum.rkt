#lang racket/base

(provide define/with-datum)

(require syntax/parse/define
         syntax/datum
         (for-syntax racket/base
                     racket/syntax
                     racket/private/sc))

(begin-for-syntax
  (define-syntax-class (fresh-temporary fresh-stx)
    #:attributes (fresh-var)
    (pattern name
             #:with fresh-var (format-id fresh-stx "~a" (generate-temporary #'name))))

  (define (count-ellipses-depth var...^n)
    (for/fold ([var...^n var...^n]
               [depth 0])
              ([current-depth (in-naturals 1)]
               #:break (not (pair? var...^n)))
      (values (car var...^n) current-depth))))

#|
Source:
  https://github.com/racket/racket/blob/8e83dc25f7f5767d9e975f20982fdbb82f62415a/racket/collects/racket/syntax.rkt#L22-#L59

  racket/collects/racket/syntax.rktracket/collects/racket/syntax.rkt
  Commit SHA: 8e83dc25f7f5767
  Line: 22-59
|#
(define-syntax-parse-rule (define/with-datum pattern rhs)
  #:attr matched-vars (get-match-vars #'define/with-datum
                                      this-syntax
                                      #'pattern
                                      '())
  #:with (((~var pvar (fresh-temporary #'here)) . depth) ...)
  (for/list ([x (attribute matched-vars)])
    (define-values (var depth)
      (count-ellipses-depth x))
    (cons var depth))

  (begin
    (define-values (pvar.fresh-var ...)
      (with-datum ([pattern rhs])
        (values (pvar-value pvar) ...)))
    (define-syntax pvar
      (make-s-exp-mapping 'depth (quote-syntax pvar.fresh-var)))
    ...))

;; auxiliary macro
(define-syntax-parse-rule (pvar-value pvar:id)
  #:attr mapping (syntax-local-value #'pvar)
  #:do [(unless (s-exp-pattern-variable? (attribute mapping))
          (raise-syntax-error #f "not a datum variable" #'pvar))]
  #:with value-var (s-exp-mapping-valvar (attribute mapping))
  value-var)
