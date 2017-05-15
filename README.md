syntax-parse-example
===
[![Build Status](https://travis-ci.org/bennn/syntax-parse-example.svg)](https://travis-ci.org/bennn/syntax-parse-example)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/syntax-parse-example/index.html)

Example macros, written using [`syntax/parse`](http://docs.racket-lang.org/syntax/Parsing_Syntax.html).


Install
---

```
  > raco pkg install syntax-parse-example
```

Or

```
  > git clone https://github.com/bennn/syntax-parse-example
  > raco pkg install ./syntax-parse-example
```


Browse
---

The documentation is online:

- <http://docs.racket-lang.org/syntax-parse-example/index.html>


And, if you installed the package, on your computer:

```
  > raco docs syntax-parse-example
```

Or, just browse this repo.
Except for the `lang/` folder, every folder here contains:

- one file implementing a macro,
- one file with unit tests, and
- one file documenting the macro


Contribute
---

To contribute a macro names `M`:

1. Clone this package (`raco pkg install --clone syntax-parse-example`)
2. Navigate to the `syntax-parse-example/` folder on your machine
2. Run `raco syntax-parse-example --new M`
3. Edit the newly-generated files:
  - `M/M.rkt`
  - `M/M-test.rkt`
  - `M/M.scrbl`
4. Add `M` to the list of examples in `index.scrbl`
5. Submit a pull request!
