#lang racket/base
(provide optional-assert exn:fail:optional-assert?)
(require (for-syntax racket/base syntax/parse))

(define-for-syntax no-asserts? (getenv "DISABLE_OPTIONAL_ASSERT"))

(struct exn:fail:optional-assert exn:fail ())

(define (make-exn:fail:optional-assert datum)
  (exn:fail:optional-assert (format "assertion failure: ~a" datum)
                            (current-continuation-marks)))

(define-syntax (optional-assert stx)
  (syntax-parse stx
   [(_ e:expr)
    (if no-asserts?
      #'(void)
      #'(unless e
          (raise (make-exn:fail:optional-assert 'e))))]))
