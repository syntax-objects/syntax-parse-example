#lang racket/base
(module+ test
  (require rackunit racket/contract syntax-parse-example/kw-ctc/kw-ctc)

  (test-case "ex0"
    (define ctc0 (kw-ctc ([a] [b symbol?] [(c 0) real?]) list?))
    (define/contract (f #:a a #:b b #:c [c 0]) ctc0 (list a b c))

    (check-not-exn
      (lambda ()
        (f #:a 'A #:b 'B #:c 3.14)))

    (check-not-exn
      (lambda ()
        (f #:a "hello" #:b 'B)))

    (check-exn exn:fail:contract?
      (lambda ()
        (f #:b 'B)))

    (check-exn exn:fail:contract?
      (lambda ()
        (f #:a (void) #:b 42)))

    (check-exn exn:fail:contract?
      (lambda ()
        (f #:a (void) #:b 'hello #:c (void)))))

  (test-case "ex1"
    (define ctc (kw-ctc ([(c 0) real?] [a] [b symbol?]) list?))
    (define/contract (f #:a a #:b b #:c [c 0]) ctc (list a b c))

    (check-not-exn
      (lambda ()
        (f #:a 'A #:b 'B)))

    (check-exn exn:fail:contract?
      (lambda ()
        (f #:a (void) #:b 'hello #:c (void)))))

)
