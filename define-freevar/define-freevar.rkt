#lang racket/base

(provide define/freevar
         with-freevar
         define/with-freevar)

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

(define-syntax (define/freevar stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        #:freevars (fv:id ...+)
        (~optional (~and #:immediate immediate-flag))
        body:expr ...+)
     #:attr dup-id (or (check-duplicate-identifier (syntax-e #'(fv ... arg ...)))
                       (cdr (check-duplicates
                             (map cons (syntax->datum #'(fv ...)) (syntax-e #'(fv ...)))
                             #:key car
                             #:default '(#f . #f))))
     #:do [(when (attribute dup-id)
             (raise-syntax-error 'define/freevar
                                 "duplicated argument or free variable name"
                                 stx
                                 (attribute dup-id)))]
     #:with name-with-fvs (format-id #'fresh-stx "~a/fvs" #'name)
     #:with immediate? (if (attribute immediate-flag) #t #f)
     #`(begin
         (define name-with-fvs
           #,(cond
               [(attribute immediate-flag)
                #`(λ (fv ...)
                    (let ([name #,(syntax/loc stx
                                    (λ (arg ...) body ...))])
                      name))]
               [else
                #`(let ([name #,(syntax/loc stx
                                  (λ (fv ... arg ...) body ...))])
                    name)]))
         (define-syntax name
           (open-term #'name-with-fvs
                      '(fv ...)
                      '(arg ...)
                      'immediate?)))]))

(define-syntax (with-freevar stx)
  (syntax-parse stx
    [(_ term-with-fv:id ([fv:id new-fv:id] ...) body:expr ...+)
     (syntax-property
      (syntax/loc stx
        (let-syntax ([term-with-fv
                      (open-term-set-freevars 'with-freevar
                                              #'term-with-fv
                                              (hash (~@ 'fv 'new-fv) ...))])
          body ...))
      'disappeared-use (list (syntax-local-introduce #'term-with-fv)))]))

(define-syntax (define/with-freevar stx)
  (syntax-parse stx
    [(_ new-name:id original-term-with-fv:id [fv:id new-fv:id] ...)
     (syntax-property
      (syntax/loc stx
        (define-syntax new-name
          (open-term-set-freevars 'with-freevar
                                  #'original-term-with-fv
                                  (hash (~@ 'fv 'new-fv) ...))))
      'disappeared-use (list (syntax-local-introduce #'original-term-with-fv)))]))

(begin-for-syntax
  (struct open-term (proc-stx freevars-name args-name immediate?)
    #:property prop:procedure (λ (self stx) (link-freevars self stx)))

  (define (freevars-in-context fvs #:context ctxt #:source src)
    (for/list ([fv (in-list fvs)])
      (syntax-property
       (format-id ctxt "~a" fv #:source src)
       'original-for-check-syntax #t)))

  (define (link-freevars self stx)
    (define/syntax-parse target (open-term-proc-stx self))
    (syntax-parse stx
      [proc-src:id
       #:with (fv ...) (freevars-in-context (open-term-freevars-name self)
                                            #:context stx
                                            #:source #'proc-src)
       #:with (arg ...) (generate-temporaries (open-term-args-name self))
       (cond
         [(open-term-immediate? self)
          (fix-app stx
                   (syntax/loc stx
                     (target fv ...)))]
         [else
          (quasisyntax/loc stx
            (λ (arg ...)
              #,(fix-app stx
                         (syntax/loc stx
                           (target fv ... arg ...)))))])]
      [(proc-src:id . args)
       #:with (fv ...) (freevars-in-context (open-term-freevars-name self)
                                            #:context stx
                                            #:source #'proc-src)
       (cond
         [(open-term-immediate? self)
          (fix-app stx
                   (quasisyntax/loc stx
                     (#,(fix-app stx
                                 (syntax/loc stx
                                   (target fv ...)))
                      . args)))]
         [else
          (fix-app stx
                   (syntax/loc stx
                     (target fv ... . args)))])]))

  (define (fix-app ctxt app-stx)
    (define app-datum (syntax-e app-stx))
    (datum->syntax ctxt app-datum app-stx app-stx))

  (define (open-term-set-freevars who open-term-id map)
    (define (fail)
      (raise-syntax-error who
                          "the binding is not defined by define/freevar"
                          open-term-id))
    (define self
      (syntax-local-value open-term-id fail))
    (unless (open-term? self)
      (fail))
    (define original-fvs (open-term-freevars-name self))
    (define new-fvs
      (for/list ([fv (in-list original-fvs)])
        (hash-ref map fv (λ () fv))))
    (open-term (open-term-proc-stx self)
               new-fvs
               (open-term-args-name self)
               (open-term-immediate? self))))

