#lang racket/base
(provide js-dict js-extract)

(require syntax/parse/define
         racket/match
         racket/hash
         racket/splicing
         (for-syntax racket/base
                     racket/list))

(begin-for-syntax
  (define-splicing-syntax-class key
    (pattern {~seq #:expr key:expr}
             #:with static #'())
    (pattern {~seq key*:id}
             #:with key #''key*
             #:with static #'(key*)))

  (define-splicing-syntax-class construct-spec
    (pattern {~seq [key:key val:expr]}
             #:with code #'`[#:set ,key.key ,val]
             #:with (static ...) #'key.static)
    (pattern {~seq #:merge e:expr}
             #:with code #'`[#:merge ,e]
             #:with (static ...) #'())
    (pattern {~seq x:id}
             #:with code #'`[#:set x ,x]
             #:with (static ...) #'(x)))

  (define-syntax-class extract-spec
    (pattern [key*:key pat:expr]
             #:with key #'key*.key
             #:with (static ...) #'key*.static)
    (pattern x:id
             #:with key #''x
             #:with pat #'x
             #:with (static ...) #'(x))))

(define (make-dict . xs)
  (for/fold ([h (hash)]) ([x (in-list xs)])
    (match x
      [`[#:set ,key ,val] (hash-set h key val)]
      [`[#:merge ,d] (hash-union h d #:combine (λ (a b) b))])))

(define-syntax-parse-rule (js-dict spec:construct-spec ...)
  #:fail-when
  (check-duplicate-identifier (append* (attribute spec.static)))
  "duplicate static key"
  (make-dict spec.code ...))

(define-syntax-parser extract
  [(_ () pat-rst rst-obj) #'(match-define pat-rst rst-obj)]
  [(_ (spec:extract-spec specs ...) pat-rst rst-obj)
   #'(splicing-let ([KEY spec.key]
                    [OBJ rst-obj])
       (match-define spec.pat (hash-ref OBJ KEY))
       (extract (specs ...) pat-rst (hash-remove OBJ KEY)))])

(define-syntax-parse-rule (js-extract (spec:extract-spec ...
                                       {~optional {~seq #:rest e:expr}})
                                      obj:expr)
  #:fail-when
  (check-duplicate-identifier (append* (attribute spec.static)))
  "duplicate static key"
  (extract (spec ...) (~? e _) obj))

