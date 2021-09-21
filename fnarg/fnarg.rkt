#lang racket/base

(provide fnarg)

(require racket/syntax syntax/parse)

;; Use _splicing_ to parse a sequence of elements for keyword args.
(define-splicing-syntax-class (fnarg [allow-optional? #t]
                                     #:keyword? [allow-keyword? #t])
  #:attributes (name fresh-var keyword default)
  #:commit
  (pattern name:id
           #:attr keyword #f
           #:attr default #f
           #:with fresh-var (generate-temporary #'name))
  (pattern [name:id default:expr]
           #:fail-unless allow-optional? "optional argument is not allowed"
           #:attr keyword #f
           #:with fresh-var (generate-temporary #'name))
  ;; The ~seq pattern describes a _sequence_ of elements
  (pattern (~seq keyword:keyword name:id)
           #:fail-unless allow-keyword? "keyword argument is not allowed"
           #:attr default #f
           #:with fresh-var (generate-temporary #'name))
  (pattern (~seq keyword:keyword [name:id default:expr])
           #:fail-unless allow-optional? "optional argument is not allowed"
           #:fail-unless allow-keyword? "keyword argument is not allowed"
           #:with fresh-var (generate-temporary #'name)))
