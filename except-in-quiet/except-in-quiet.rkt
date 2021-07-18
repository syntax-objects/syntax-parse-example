#lang racket/base
(provide except-in-quiet)

(require
  (for-syntax
    racket/base
    racket/require-transform
    racket/set
    syntax/parse))

(define-for-syntax (export-list->phase0-ids phase-dict)
  (define phase0-id-info*
    (cdr
      (or (assoc 0 phase-dict)
          (cons #f '()))))
  (map car phase0-id-info*))

(define-syntax except-in-quiet
  (make-require-transformer
    (syntax-parser
      [(_ modname:id exception-names:id ...)
       (define-values [provided-vals provided-macros]
         (module->exports (syntax-e #'modname)))
       (define exceptions
         (map syntax-e (attribute exception-names)))
       (define excluded
         (set-intersect
           exceptions
           (set-union
             (export-list->phase0-ids provided-vals)
             (export-list->phase0-ids provided-macros))))
       (expand-import #`(except-in modname #,@excluded))])))

