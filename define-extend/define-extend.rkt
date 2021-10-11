#lang racket/base
(provide define-extend)

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/lib/function-header))

(begin-for-syntax
  (struct extensible (closed-id open-id)
    #:property prop:rename-transformer 0)

  (define-splicing-syntax-class extend-option
    #:attributes (parent-id open-id)
    (pattern (~seq #:extend parent-id:id)
             #:do [(define-values (parent-ext _)
                     (syntax-local-value/immediate #'parent-id
                                                   (λ () (values #f #f))))]
             #:fail-when (and (not (extensible? parent-ext)) #'parent-id)
             "expected an extensible procedure"
             #:attr open-id (extensible-open-id parent-ext))
    (pattern (~seq)
             #:attr parent-id #f
             #:attr open-id #f)))

(define-syntax (define-extend stx)
  (syntax-parse stx
    [(_ (?name:id . ?fmls:formals) ?ext:extend-option ?body:expr ...+)
     #:with (?closed ?open) (generate-temporaries #'(?name ?name))
     #:with ?proc
     (syntax/loc stx
       (~? (λ ?fmls
             (let ([?ext.parent-id (?ext.open-id ?name)])
               ?body ...))
           (λ ?fmls ?body ...)))
     #'(begin
         (define ?closed
           (letrec ([?name ?proc])
             ?name))
         (define (?open ?name) ?proc)
         (define-syntax ?name
           (extensible #'?closed #'?open)))]))

