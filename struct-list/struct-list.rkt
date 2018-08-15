#lang typed/racket/base
(provide struct-list)
(require (for-syntax racket/base racket/syntax syntax/parse))

(define-syntax (struct-list stx)
  (syntax-parse stx #:datum-literals (:)
   [(_ name:id ([f*:id : t*] ...) #:type-name Name:id)
    #:fail-when (free-identifier=? #'name #'Name)
                "struct name and #:type-name must be different"
    #:with name?
           (format-id stx "~a?" (syntax-e #'name))
    #:with ((name-f* i*) ...)
           (for/list ([f (in-list (syntax-e #'(f* ...)))]
                      [i (in-naturals 1)])
             (list (format-id stx "~a-~a" (syntax-e #'name) (syntax-e f)) i))
    (syntax/loc stx
      (begin
        (define-type Name (Pairof 'name (Listof Any)))
        (define (name (f* : t*) ...) : Name
          (list 'name f* ...))
        (define (name? (v : Any)) : Boolean
          (and (list? v) (not (null? v)) (eq? 'name (car v))))
        (define (name-f* (p : Name)) : t*
          (cast (list-ref p 'i*) t*))
        ...))]))
