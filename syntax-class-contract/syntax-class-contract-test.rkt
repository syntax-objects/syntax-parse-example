#lang racket/base
(module+ test
  (require rackunit
           syntax/macro-testing
           (for-syntax
             racket/base
             racket/contract
             (only-in syntax/parse str)
             (only-in syntax/parse/experimental/reflect reify-syntax-class)
             syntax-parse-example/syntax-class-contract/syntax-class-contract))

  (test-case "str"

    (define-syntax add-hello
      (contract
        (-> (syntax-class-contract (reify-syntax-class str))
            syntax?)
        (lambda (stx)
          (let ([orig-str (syntax-e (cadr (syntax-e stx)))])
            (with-syntax ([new-str (string-append "hello" " " orig-str)])
              #'new-str)))
        'this-macro
        'the-macro-user))

    (check-equal? (add-hello "world") "hello world")

    (check-exn exn:fail?
      (lambda () (convert-compile-time-error (add-hello 42)))))
)
