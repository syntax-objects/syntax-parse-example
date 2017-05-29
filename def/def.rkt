#lang racket/base
(provide def)
(require rackunit (for-syntax racket/base syntax/parse))

(begin-for-syntax
  (define-syntax-class arg-spec
    #:attributes (name type)
    #:datum-literals (:)
    (pattern
      (name:id : type:expr))
    (pattern
     name:id
     #:with type #'#f))

  (define-syntax-class doc-spec
    (pattern
      e:str))
)

(define-syntax (def stx)
  (syntax-parse stx #:datum-literals (==>)
   [(_ (name:id arg*:arg-spec ...)
      (~or ;; using (~or (~once a) ...) to simulate an unordered (~seq a ...)
        (~once (~describe #:role "docstring" "docstring" doc:doc-spec))
        (~once (~seq #:test ((in* ==> out*
                             (~optional (~seq #:stdout expected-output*:str)
                                        #:defaults ([expected-output* #'#f])))
                            ...)))
        (~once (~optional (~seq #:pre ([check-pre* pre-doc*:doc-spec] ...))
                          #:defaults ([(check-pre* 1) '()] [(pre-doc* 1) '()])))
        (~once (~optional (~seq #:post ([check-post* post-doc*:doc-spec] ...))
                          #:defaults ([(check-post* 1) '()] [(post-doc* 1) '()])))) ...
       body)
    #:with check-types
      #'(for ([arg-name (in-list (list arg*.name ...))]
              [arg-type (in-list (list arg*.type ...))]
              [i        (in-naturals)]
              #:when arg-type)
          (unless (arg-type arg-name)
            (raise-argument-error 'name (symbol->string (object-name arg-type)) i arg-name)))
    #:with check-pre
      #'(for ([pre-check (in-list (list check-pre* ...))]
              [pre-doc   (in-list (list pre-doc* ...))])
          (unless (pre-check arg*.name ...)
            (raise-user-error 'name pre-doc)))
    #:with check-post
      #'(lambda (result)
          (for ([post-check (in-list (list check-post* ...))]
                [post-doc   (in-list (list post-doc* ...))])
            (unless (post-check result)
              (error 'name post-doc))))
    #:with test-cases
      #'(module+ test
          (let* ([p (open-output-string)]
                 [result-val (parameterize ([current-output-port p]) in*)]
                 [result-str (begin0 (get-output-string p)
                                     (close-output-port p))])
              (check-equal? result-val out*)
              (when expected-output*
                (check-equal? result-str expected-output*)))
          ...)
    #'(begin
        test-cases
        (define (name arg*.name ...)
          check-types
          check-pre
          (let ([result body])
            (begin0 result
                    (check-post result)))))]))
