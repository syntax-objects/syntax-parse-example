#lang racket/base
(module+ test
  (require rackunit syntax/macro-testing
           (for-syntax racket/base syntax/parse syntax/transformer syntax-parse-example/fresh-variable/fresh-variable))

  (define-syntax (define/immutable-parameter stx)
    (syntax-parse stx
      [(_ (name:id (~or* (~and _:id arg:fresh-variable)
                         [(~and _:id arg:fresh-variable) default-value:expr])
                   ...)
          body:expr ...+)
       #'(define (name (~? [arg.fresh-var default-value]
                           arg.fresh-var) ...)
           ;; disable set! on arg
           (define-syntax arg
             (make-variable-like-transformer #'arg.fresh-var #f))
           ...
           body ...)]))

  (check-exn #rx"set!: cannot mutate identifier"
    (lambda ()
      (convert-compile-time-error
        (let ()
          (define/immutable-parameter (bad-fn n)
            (set! n 12345) ;=> syntax error
            (void))
          (void)))))

  (check-exn #rx"set!: cannot mutate identifier"
    (lambda ()
      (convert-compile-time-error
        (let ()
          (define/immutable-parameter (bad-fn-optional n [verbose? #f])
            (set! verbose? 12345) ;=> syntax error
            (void))
          (void)))))

  (define/immutable-parameter (fib n [verbose? #f])
    (when verbose?
      (printf "(fib ~a)\n" n))
    (cond
      [(<= n 1) n]
      [else
       (+ (fib (- n 1) verbose?)
          (fib (- n 2) verbose?))]))

  (check-equal? (fib 4) 3)
  (check-equal? (fib 8) 21)

)
