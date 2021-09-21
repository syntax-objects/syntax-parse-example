#lang racket/base
(module+ test
  (require rackunit syntax/macro-testing
           (for-syntax racket/base syntax/parse syntax/transformer syntax-parse-example/fnarg/fnarg))

  ;; macro for defining functions that cannot mutate their parameters and do
  ;;  not accept keyword arguments
  (define-syntax (define/immutable-parameter stx)
    (syntax-parse stx
      [(_ (name:id (~var arg (fnarg #:keyword? #f)) ...) body:expr ...+)
       ;; The `~@` form _splices_ the keyword at (A), if it exists, into the
       ;;  function header.
       ;; The `~?` form at (B) chooses the first form if both the arg.fresh-var
       ;;  and arg.default attributes have values. Otherwise, `~?` chooses the
       ;;  second form.
       #'(define (name (~@ (~? arg.keyword)                ;; (A)
                           (~? [arg.fresh-var arg.default] ;; (B)
                               arg.fresh-var))
                       ...)
           ;; Disable set! for arg
           (define-syntax arg.name
             (make-variable-like-transformer #'arg.fresh-var #f))
           ...
           body ...)]))

  (check-exn #rx"set!: cannot mutate identifier"
    (lambda ()
      (convert-compile-time-error
        (let ()
          (define/immutable-parameter (bad-fn x)
            (set! x 'oops)
            (void))
          (void)))))

  (check-exn #rx"keyword argument is not allowed"
    (lambda ()
      (convert-compile-time-error
        (define/immutable-parameter (has-kw a #:cache cache?)
          (void)))))

  (test-case "fib/cache"
    (define fib-cache (make-hash))
    (define/immutable-parameter (fib n [cache #f])
      (cond
        [(<= n 1) n]
        [(and cache (hash-has-key? cache n))
         (hash-ref cache n)]
        [else
         (define result
           (+ (fib (- n 1) cache)
              (fib (- n 2) cache)))
         (when cache
           (hash-set! cache n result))
         result]))

    (check-equal? (fib 8) 21)
    (check-equal? (fib 40 fib-cache) 102334155))
)
