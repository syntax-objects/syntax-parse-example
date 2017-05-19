#lang racket
(require rackunit syntax-parse-example/cross-macro-communication/cross-macro-communication)

(define-for-macros cake 42)
(check-equal? (get-macroed cake) 42)
(check-exn exn:fail:syntax?
           (λ () (eval #'cake)))

