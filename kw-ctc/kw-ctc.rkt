#lang racket/base
(provide kw-ctc)
(require racket/contract (for-syntax racket/base racket/list syntax/parse syntax/parse/experimental/template))

(begin-for-syntax

  (define (id->keyword stx)
    (string->keyword (symbol->string (syntax-e stx))))

  (define-syntax-class field
    (pattern [id:id (~optional cont:expr)]
             #:with required? #'#t
             #:with field-contract (template (?? cont any/c))
             #:with kw #`#,(id->keyword #'id))
    (pattern [(id:id _:expr) (~optional cont:expr)]
             #:with required? #'#f
             #:with field-contract (template (?? cont any/c))
             #:with kw #`#,(id->keyword #'id)))

  (define field->required?
    (syntax-parser [f:field (syntax-e #'f.required?)]))

  (define field->kw
    (syntax-parser [f:field (syntax/loc this-syntax f.kw)]))

  (define field->ctc
    (syntax-parser [f:field (syntax/loc this-syntax f.field-contract)]))

  (define (field*->contract-spec field*)
    (apply append (map (lambda (f) (list (field->kw f) (field->ctc f))) field*)))
)

(define-syntax (kw-ctc stx)
  (syntax-parse stx
   [(_ (?dom*:field ...) cod)
    (define-values [mandatory* optional*]
      (partition field->required? (syntax-e #'(?dom* ...))))
    (with-syntax ([mandatory-ctc-spec #`#,(field*->contract-spec mandatory*)]
                  [optional-ctc-spec #`#,(field*->contract-spec optional*)])
      (syntax/loc stx
        (->* mandatory-ctc-spec optional-ctc-spec cod)))]))

