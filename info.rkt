#lang info
(define collection "syntax-parse-example")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc"))
(define pkg-desc "Examples of syntax/parse")
(define version "0.0")
(define pkg-authors '(ben))
(define scribblings '(("syntax-parse-example.scrbl" () ("Examples"))))
(define raco-commands '(("syntax-parse-example" (submod syntax-parse-example/raco main) "macro-management" #f)))
