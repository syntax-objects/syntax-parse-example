#lang racket/base

(provide try catch catch/match finally)

(require (for-syntax racket/base)
         racket/match
         syntax/parse/define)

(begin-for-syntax
  (define ((only-in-try name) stx)
    (raise-syntax-error name "not allowed except in try" stx)))

(define-syntax catch (only-in-try 'catch))
(define-syntax catch/match (only-in-try 'catch/match))
(define-syntax finally (only-in-try 'finally))

(begin-for-syntax
  (define-syntax-class try-body
    #:literals (catch catch/match finally)
    (pattern {~and :expr {~not {~or (catch . _) (catch/match . _) (finally . _)}}}))

  (define-syntax-class catch-clause
    #:attributes ((handlers 1))
    #:literals (catch)
    (pattern (catch [pred:expr name:id body:expr ...+] ...)
             #:with (handlers ...) #'([pred (λ (name) body ...)] ...)))

  ;; this one's for you, notjack
  (define-syntax-class catch-match-clause
    #:attributes (handler)
    #:literals (catch/match)
    (pattern (catch/match [clause:expr body:expr ...+] ...)
             #:with (match-clauses ...) #'([clause body ...] ...)
             #:with handler #'[(λ (_) #t) ;; catch 'em all
                               (match-lambda
                                 match-clauses ...
                                 ;; rethrow as last resort
                                 [e (raise e)])]))

  (define-syntax-class finally-clause
    #:attributes (handler)
    #:literals (finally)
    (pattern (finally body:expr ...+)
             #:with handler #'(λ () body ...))))

;; Calls value-thunk, then post-thunk, with post-thunk guaranteed to be run
;; even if execution exits value-thunk through an exception or continuation
;;
;; value-thunk is prevented from re-entry and continutation shenanigans by a
;; continuation-barrier
;;
;; thanks to Alex Knauth & SamPh on Discord
(define (call-with-try-finally value-thunk post-thunk)
  (call-with-continuation-barrier
    (λ () (dynamic-wind void value-thunk post-thunk))))

(define-syntax-parser try
  [(_ body:try-body ...+
      {~optional c:catch-clause}
      {~optional m:catch-match-clause}
      {~optional f:finally-clause})
   #'(call-with-try-finally
       (λ ()
         (with-handlers ((~? (~@ c.handlers ...))
                         (~? m.handler))
           body ...))
       (~? f.handler void))])

