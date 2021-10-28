#lang racket/base

(require racket/function
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     racket/format
                     syntax/parse))

(provide log-once)

(define log-var (curry printf "~a = ~s. "))
(define-syntax-parser log-def
  [(_ expr:expr)
   #`(log-var #,(~s (syntax->datum #'expr)) expr)])

(begin-for-syntax
  (define (make-incrementor id)
    (with-syntax ([id id])
      #'(λ ()
          (set! id (add1 id))
          id))))

(define-syntax-parser log-defs
  [(_ (~optional (~seq #:newline use-newline-stx:boolean))
      exprs*:expr ...+)
   #:attr use-newline (syntax-e #'(~? use-newline-stx #f))
   #:attr intermediate-newline-clause (if (attribute use-newline) #'(newline) #f)
   #:attr ultimate-newline-clause (if (attribute use-newline) #f #'(newline))
   #'(begin
       (~@ (log-def exprs*)
           (~? intermediate-newline-clause)) ...
       (~? ultimate-newline-clause))])

(define-syntax-parser log-once 
  [(_ (~alt (~optional (~seq #:skip-count target-skip-count:nat) #:name "#:skip-count keyword"
                       #:defaults ([target-skip-count #'0]))
            (~optional (~seq #:log-count target-log-count:nat) #:name "#:log-count keyword"
                       #:defaults ([target-log-count #'1]))
            (~optional (~seq #:when condition:expr) #:name "#:when keyword")
            (~optional (~seq #:message message:str) #:name "#:message keyword")
            (~optional (~seq #:newline newln:boolean) #:name "#:newln keyword")) ...
      exprs* ...+)
   #:with logged (syntax-local-lift-expression #'#f)
   #:with run-count (syntax-local-lift-expression #'0)
   #:with ++run-count (make-incrementor #'run-count)
   #:with log-count (syntax-local-lift-expression #'0)
   #:with ++log-count (make-incrementor #'log-count)
   #:with should-run?! (syntax-local-lift-expression
                        #'(λ ()
                            (and (> (++run-count) target-skip-count)
                                 (<= (++log-count) target-log-count))))
   #:with stop-logging?! (syntax-local-lift-expression
                          #'(λ ()
                              (when (<= target-log-count log-count)
                                (set! logged #t))))
   #'(and (not logged)
          (when (and (~? condition)
                     (should-run?!))
            (~? (display message))
            (log-defs (~? (~@ #:newline newln)) exprs* ...)
            (stop-logging?!)))])
