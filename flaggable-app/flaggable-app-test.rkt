#lang racket/base
(module+ test
  (require rackunit syntax/macro-testing syntax-parse-example/flaggable-app/flaggable-app)

  (test-case "example"
    (define (f c #:a [a #f] #:b [b #f])
      (list c a b))
    (check-equal? (f 0 #:a #:b)       '(0 #t #t))
    (check-equal? (f 0 #:a)           '(0 #t #f))
    (check-equal? (f 0 #:b)           '(0 #f #t))
    (check-equal? (f 0 #:a 10 #:b)    '(0 10 #t))
    (check-equal? (f 0 #:a #:b 20)    '(0 #t 20))
    (check-equal? (f 0 #:a 10 #:b 20) '(0 10 20))
    (check-equal? (f 0)               '(0 #f #f))
    (check-exn exn:fail:syntax?
      (lambda () (convert-compile-time-error (f #:a 0 1)))))

)
