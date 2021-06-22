#lang scribble/manual
@require[
  syntax-parse-example/render
  (for-label
    racket/base
    scribble/manual
    syntax/parse
    syntax-parse-example/first-class-or/first-class-or)
]
@(define GITHUB-URL "https://github.com/bennn/syntax-parse-example")

@title{Syntax Parse Examples}
Source code: @url[GITHUB-URL]

@defmodulelang[syntax-parse-example]{
  This package is a collection of useful and/or illustrative macros written
  using the @racketmodname[syntax/parse] library.

  The @secref{sec:reference} section documents the @racketmodname[syntax-parse-example] language.
}


@; =============================================================================
@section{How to browse the examples}

Two options:
@itemlist[
@item{
  Scroll through this document, read the macros' source code and look at the
  example uses of each macro.
}
@item{@(let ([example-macro-name @tt{big-mac}]) @list{
  The source code for each macro is in a top-level folder at @url[GITHUB-URL].
  For example, the source for a macro named @|example-macro-name| would be in
  the folder @tt{@|GITHUB-URL|/@|example-macro-name|}.
})}
]


@section{How to use the examples in another project}

Three options, ordered from best to worst.
@itemlist[
@item{
  Copy/paste the example code into a new file in your project, require that
  new file normally.
}
@item{@(let ([example-macro-name @tt{first-class-or}]
             [example-macro-modname @filepath{syntax-parse-example/first-class-or/first-class-or}]) @list{
  Install the @racketmodname[syntax-parse-example] package, then require
  the macro's defining module.
  For example, the defining module for the @|example-macro-name| macro is
  @|example-macro-modname|.
})}
@item{
  Clone the @hyperlink[GITHUB-URL]{source code}, then require the
  module path of the file that defines the macro.
}
]


@section{How to contribute a new example}
@(let ([example-macro-name "EUGENE"]) @list{

To create an example named @tt[example-macro-name]:
@itemlist[
@item{
  Clone this repository (@hyperlink[GITHUB-URL]{link}).
}
@item{
  Run @exec{raco syntax-parse-example --new @|example-macro-name|} in the
  top-level folder of the cloned repository.
  This generates three new files:
  @itemlist[
  @item{
    @tt{@|example-macro-name|/@|example-macro-name|.rkt} (source code)
  }
  @item{
    @tt{@|example-macro-name|/@|example-macro-name|-test.rkt} (tests)
  }
  @item{
    @tt{@|example-macro-name|/@|example-macro-name|-doc.scrbl} (Scribble documentation)
  }
  ]
}
@item{
  Fill the holes in the newly-generated files with an implementation,
   some unit tests,
   and documentation.
}
@item{
  Run @exec{raco setup syntax-parse-example} to generate the documentation.
}
]

})


@section{Tutorials}

Where to learn about @racket[syntax-parse]?

@itemlist[
@item{
  Official documentation, @emph{Syntax: Meta-Programming Helpers}.
  @hyperlink["https://docs.racket-lang.org/syntax/index.html"]{docs.racket-lang.org/syntax/index.html}
}
@item{
  @emph{Mythical Macros}.
  @hyperlink["https://soegaard.github.io/mythical-macros/"]{soegaard.github.io/mythical-macros}
}
@item{
  @emph{Macros and Languages in Racket}.
  @hyperlink["http://rmculpepper.github.io/malr/index.html"]{rmculpepper.github.io/malr/index.html}
}
]

The @hyperlink["https://www.greghendershott.com/fear-of-macros/"]{Fear of
Macros} tutorial is also very good, but was written before the age of syntax
parse.


@section{A @racket[syntax-parse] Crash Course}

The @racket[syntax-parse] form is a tool for un-packing data from a
 @tech/reference{syntax object}.
It is similar to Racket's @racket[match].
Since the input to a @tech/guide{macro} is always a syntax object,
 @racket[syntax-parse] is helpful for writing macros.

A syntax object is a Racket representation of source code.
For example, @racket[#'(+ 1 2)] is a syntax object that represents the
 sequence of characters @litchar{(+ 1 2)}, along with the information that
 the @racket[+] identifier is bound to a function in the @racketmodname[racket/base]
 library.

A macro is a compile-time function on syntax objects.
In other words, a macro:
 (1) is a function,
 (2) expects a syntax object as input,
 (3) returns a new syntax object, and
 (4) runs at compile-time (see @secref["expansion" #:doc '(lib "scribblings/reference/reference.scrbl")]).

Here is a simple macro that expects two arguments and returns its first argument.
Note that when the expander finds a macro application, it invokes the macro with a syntax object representing the whole application (see also: @secref["macro-transformers" #:doc '(lib "scribblings/guide/guide.scrbl")]).
@margin-note{The name @racket[_K] is historic (@hyperlink["http://wiki.c2.com/?EssAndKayCombinators"]{link}) and pretentious, enjoy.}

@examples[#:eval (make-base-eval)
(require (for-syntax racket/base))
(define-syntax (K args-stx)
  (define args (syntax-e args-stx))
  (if (= (length args) 3)
    (cadr args)
    (raise-argument-error
      'K
      "syntax object containing a list with 3 elements"
      args-stx)))
(K 1 2)
(eval:error (K 1))
(eval:error (K 1 2 3))
]

Here is the same macro, defined using @racket[syntax-parse] instead of the low-level
 @racket[syntax-e] and @racket[cadr] functions:

@examples[#:eval (make-base-eval)
(require (for-syntax racket/base syntax/parse))
(define-syntax (K args-stx)
  (syntax-parse args-stx
   [(_ ?arg0 ?arg1)
    #'?arg0]))
(K 1 2)
(eval:error (K 1))
(eval:error (K 1 2 3))
]

I don't expect that all this makes sense so far.
Try running and modifying these examples.
Try reading the documentation for @racket[define-syntax] and @racket[syntax-e]
 and @racket[syntax-parse] and @racket[syntax] (aka @litchar{#'}).

But the last thing to point out is that @racket[(_ _?arg0 _?arg1)] is a
 @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{syntax pattern}.
@itemlist[
@item{
  the parentheses say this pattern matches a (special kind of) list,
}
@item{
  the underscore (@litchar{_}) means the first element of the list can be anything,
}
@item{
  the name @racket[_?arg0] means the second element of the list can be anything
   and gets bound to the @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{pattern variable}
   @racket[_?arg0],
}
@item{
  the name @racket[_?arg1] binds the third element to another pattern variable,
}
@item{
  and if the list has more or fewer elements the pattern does not match.
}
]

A pattern variable is a special kind of variable; it can only be referenced inside
 a new syntax object.
The name @racket[_?arg0] starts with a @litchar{?} as a style choice --- it helps me remember that it is the name of a pattern variable.


@; =============================================================================
@include-section{index.scrbl}
@; =============================================================================

@section[#:tag "sec:reference"]{Example-Formatting Tools}

The @racketmodname[syntax-parse-example] language is a small language for
 documenting example macros. It:
@itemlist[
@item{
  uses the reader from @racketmodname[scribble/base]; and
}
@item{
  provides a few utility functions, documented below.
}
]

@defmodule[syntax-parse-example/render]{
  Helpers for rendering documentation.
}

@defproc[(tech/guide [pre-content pre-content?] ...) element?]{
  Similar to @racket[tech], but links to @other-manual['(lib "scribblings/guide/guide.scrbl")].
}

@defproc[(tech/reference [pre-content pre-content?] ...) element?]{
  Similar to @racket[tech], but links to @other-manual['(lib "scribblings/reference/reference.scrbl")].
}

@defproc[(racketfile [filename path-string?]) element?]{
  Typesets the contents of the given file as if its contents were wrapped in a @racket[racketblock].
}
