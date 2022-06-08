#lang syntax-parse-example
@require[
  (for-label racket/base syntax/parse/define syntax/parse syntax-parse-example/js-dict/js-dict)]

@(define js-dict-eval
   (make-base-eval '(require syntax-parse-example/js-dict/js-dict)))

@(define (codeverb . elem*) (nested #:style 'code-inset (apply verbatim elem*)))

@title{JavaScript-Inspired Dictionary Syntax}
@stxbee2021["sorawee" 17]

JavaScript (JS) has really elegant syntax to manipulate dictionaries.


@bold{JS Dictionary Creation}

Given @tt{x = 42} the following syntax makes a dictionary with four entries:

@codeverb|{
 {a: 1 + 2, b: 3, ['a' + 'b']: 4, x}
}|

@itemlist[
  @item{@tt{'a'} maps to @tt{3};}
  @item{@tt{'b'} maps to @tt{3};}
  @item{@tt{'ab'} maps to @tt{4}; and}
  @item{@tt{'x'} maps to @tt{42}}
]

@bold{JS Dictionary Merging}

Other dictionaries can be merged as a part of dictionary creation.

Given:

@codeverb{
let a = {a: 1, c: 2};
let b = {b: 2, c: 3};
}

Then the following dictionary has four entries:

@codeverb{
{b: 42, ...a, ...b, a: 4, d: 5}
}

@itemlist[
@item{@tt{'a'} maps to @tt{4};}
@item{@tt{'b'} maps to @tt{2};}
@item{@tt{'c'} maps to @tt{3}; and}
@item{@tt{'d'} maps to @tt{5}}
]

Note that the merging syntax can be used to set a value functionally without
mutating the dictionary.

@bold{JS Dictionary Extraction}

Given:

@codeverb{
let x = {a: 1, b: 2, c: 3, d: 4};
}

Then the following syntax:

@codeverb{
`let {a, b: bp} = x;`
}

binds @tt{a} to @tt{1} and @tt{bp} to @tt{2}.



@bold{JS Dictionary Extraction of the rest}

As a part of extraction, there can be at most one @tt{...}, which will function as
the extraction of the rest

For example:

@codeverb{
  let {a, b: bp, ...y} = x;
}

binds @tt{a} to @tt{1}, @tt{bp} to @tt{2}, @tt{y} to @tt{{c: 3, d: 4}}.


@; =============================================================================

@defmodule[syntax-parse-example/js-dict/js-dict]{}

The @racket[js-dict] and @racket[js-extract] macros bring these operations to
Racket, using immutable hash tables as the data structure.
Additionally, the @racket[js-extract] macro improves upon JS by supporting
arbitrary match pattern.

@defform[(js-dict construct-spec ...)
         #:grammar ([ccnstruct-spec [key expr]
                                    (code:line #:merge expr)
                                    id]
                    [key (code:line #:expr expr)
                         id])]{

  @examples[#:eval js-dict-eval
    (define d 4)
    (define base-1 (js-dict [x '((10))] [b 20]))
    (define base-2 (js-dict [y 30] [a 40]))
    (define obj
      (js-dict
       [a 1]
       #:merge base-1
       [b 2]
       #:merge base-2
       [#:expr (string->symbol "c") 3]
       d))
     obj
  ]

}

@defform[(js-extract (extract-spec ... maybe-rest) obj-expr)
         #:grammar ([extract-spec [key pattern-expr]
                                  id]
                    [maybe-rest (code:line)
                                (code:line #:rest expr)]
                    [key (code:line #:expr expr)
                         id])]{

  With the above @racket[_obj], in the following code adds five definitions:

  @examples[#:eval js-dict-eval
    (js-extract ([#:expr (string->symbol "a") f]
                 c
                 d
                 [x (list (list x))]
                 #:rest rst)
                obj)
    f
    c
    d
    x
    rst
  ]
}

Implementation:

@racketfile{js-dict.rkt}

