#lang syntax-parse-example

@; https://github.com/syntax-objects/Summer2021/issues/10
@; https://github.com/syntax-objects/Summer2021/issues/12

@title{Try/Catch/Finally Macros}

@tt{try}/@tt{catch}/@tt{finally} is a common and familiar syntax for handling
exceptions, used in many languages such as Java, C++ and Clojure. Errors thrown
within the try block may be "caught" by the catch clauses. In any case, whether
by normal return or exception, the finally clause is executed.

@include-section{1.scrbl}
@include-section{2.scrbl}
@include-section{3.scrbl}
