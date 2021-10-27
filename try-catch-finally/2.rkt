#lang racket/base

(provide try catch finally => _)

(require syntax/parse/define (for-syntax racket/base))

(begin-for-syntax
  (define (not-allowed-as-an-expression stx)
    (raise-syntax-error #f "not allowed as an expression" stx))

  (define-syntax-class finally-clause #:literals [finally]
    [pattern (finally e:expr ...+) #:with post-thunk #'(λ () e ...)])

  (define-syntax-class catch-clause #:literals [catch => _]
    [pattern (catch pred:expr => handler:expr)]
    [pattern (catch (pred:expr x:id) b:expr ...+)
      #:with handler #'(λ (x) b ...)]
    [pattern (catch (x:id) b:expr ...+)
      #:with pred #'void
      #:with handler #'(λ (x) b ...)]
    [pattern (catch _ b:expr ...+)
      #:with pred #'void
      #:with handler #'(λ (x) b ...)])

  (define-syntax-class body #:literals [finally catch]
    [pattern {~and :expr {~not {~or (finally . _) (catch . _)}}}]))

(define-syntax catch not-allowed-as-an-expression)
(define-syntax finally not-allowed-as-an-expression)

(define-syntax-parser try
  [(_ b:body ...+ f:finally-clause)
   #'(call-with-try-finally (λ () b ...) f.post-thunk)]
  [(_ b:body ...+ c:catch-clause ...)
   #'(with-handlers ([c.pred c.handler] ...) b ...)]
  [(_ b:body ...+ c:catch-clause ... f:finally-clause)
   #'(call-with-try-finally
      (λ () (with-handlers ([c.pred c.handler] ...) b ...))
      f.post-thunk)])

;; call-with-try-finally : [-> X] [-> Any] -> X
;; Calls value-thunk, then post-thunk, with post-thunk guaranteed to be run
;; even if execution exits value-thunk through an exception or continuation
(define (call-with-try-finally value-thunk post-thunk)
  (call-with-continuation-barrier
   (λ () (dynamic-wind void value-thunk post-thunk))))

