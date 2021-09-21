#lang racket/base
(module+ test
  (require rackunit syntax-parse-example/define-freevar/define-freevar)

  (test-case "who-error"
    (define/freevar (raise-who-error message source-stx)
      #:freevars (who)
      (raise-syntax-error who
                          message
                          source-stx))
    (check-exn #rx"knock-knock: who"
      (lambda ()
        (let ([who 'knock-knock])
          (raise-who-error "who's there" #'door)))))

  (test-case "fib-immediate"
    (define/freevar (fib n)
      #:freevars (init0 init1)
      #:immediate
      (for/fold ([a init0]
                 [b init1]
                 [fib-list '()]
                 #:result (reverse fib-list))
                ([i (in-range n)])
        (values b (+ a b) (cons a fib-list))))

    (define init0 2)

    (check-pred
      procedure?
      (let ([init1 13])
        fib))            ;; <- The #:immediate flag makes a difference

    (check-equal?
      ;; init0 shadows the global definition
      ;;=> '(0 1 1 2 3 5 8 ...)
      (let ([init0 0]
            [init1 1])
        (fib 10))
      '(0 1 1 2 3 5 8 13 21 34))

    (check-equal?
      ;; The free variable init1 is renamed to b
      (with-freevar fib ([init1 b])
        (define b 4)
        (fib 10))
      '(2 4 6 10 16 26 42 68 110 178))

    (check-equal?
      ;; Another renaming example. Free variables do not have bindings.
      (let ([b 5])
        (with-freevar fib ([init1 b])
          (fib 10)))
      '(2 5 7 12 19 31 50 81 131 212))

    ;; Define a new open term, fib-same, with free variables renamed from fib.
    (define/with-freevar fib-same fib
      [init0 S]
      [init1 S])

    (check-equal?
      (let ([S 3])
        (fib-same 10))
      '(3 3 6 9 15 24 39 63 102 165)))

)
