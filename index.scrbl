#lang scribble/manual
@require[(for-syntax racket/base syntax/parse)]
@title{The Examples}

@(define-syntax (include-example stx)
   (syntax-parse stx
    [(_ ?name:str)
     #:with scrbl-path-str (let ([name (syntax-e #'?name)])
                             (path->string
                               (path-add-extension (build-path name name) #".scrbl")))
     #:fail-unless (file-exists? (syntax-e #'scrbl-path-str))
                   (format "file '~a' does not exist, cannot render example"
                           (syntax-e #'scrbl-path-str))
     (syntax/loc stx
       (include-section scrbl-path-str))]))

@; =============================================================================

@include-example{first-class-or}
@include-example{cross-macro-communication}
@include-example{let-star}
@include-example{def}
@include-example{conditional-require}
@include-example{multi-check-true}
@include-example{define-datum-literal-set}
@include-example{rec-contract}
@include-example{struct-list}
