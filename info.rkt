#lang info
(define collection "syntax-parse-examples")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc"))
(define pkg-desc "Examples of syntax/parse")
(define version "0.0")
(define pkg-authors '(ben))
(define scribblings '(("syntax-parse-examples.scrbl" () ("Examples"))))
(define raco-commands '(("syntax-parse-examples" (submod syntax-parse-examples/main main) "macro-management" #f)))
