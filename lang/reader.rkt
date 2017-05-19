#lang s-exp scribble/base/reader
syntax-parse-example/render
#:wrapper1 (lambda (t) (cons 'doc (t)))
