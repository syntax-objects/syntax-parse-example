#lang syntax-parse-example
@require[
  (for-label racket/base racket/contract/base syntax/parse syntax-parse-example/marc-matcher/marc-matcher)]

@(define marc-matcher-eval
   (make-base-eval '(require racket/string racket/list syntax-parse-example/marc-matcher/marc-matcher)))

@(define (subitem . elem*) (itemlist (apply item elem*)))

@title{@tt{marc-matcher}}
@stxbee2021["hzafar" 4]

@; =============================================================================

@defmodule[syntax-parse-example/marc-matcher/marc-matcher]{}

This is a very domain-specific macro, developed for a particular bibliographic
metadata use-case.

@defform[(marc-matcher ([re #:as name] ...) body ...)]{
  This macro aims to make it easier to do regex-like matching over a structured
  bibliographic data format known as
  @hyperlink["https://www.loc.gov/marc/bibliographic/" "MARC 21"].
  MARC records contain a sequence of fields whose data are string values that
  look like this:

  @nested[#:style 'code-inset
    @tt{$aCarroll, Lewis,$d1832-1898,$eauthor.}]

  In each field, individual subfields are separated using a separator character
  (in this case @tt{$}); the character immediately following the separator is
  called the subtag; and the substring up to the next separator or end-of-string
  is the subfield data. So in the example above, there are three subfields,
  @tt{$a}, @tt{$d}, and @tt{$e}, whose data are, respectively, @tt{Carroll, Lewis},
  @tt{1832-1898}, and @tt{author}.

  Parsing subfields out of this is often done using regular expressions, but it
  gets really difficult when trying to deal with subfield repetitions. I'll use
  @hyperlink["https://www.loc.gov/marc/bibliographic/bd264.html" "field 264"]
  to illustrate.
  This field mainly contains the following pieces of publication information:
  the @tt{$a} subfield contains place of publication; the @tt{$b} subfield
  contains the entity responsible for publication; and the @tt{$c} subfield
  contains the date of publication.
  There are several possible repetition patterns for these subfields which
  require different semantic interpretations. To give a few examples:

  @itemlist[
    @item{
      @tt{a+bc}: multiple places of publication with the same publisher
      @subitem{@tt{$aLondon ;$aNew York :$bRoutledge,$c2017.} [@hyperlink["https://catalog.loc.gov/vwebv/staffView?bibId=19255280" "source"]]}
    }
    @item{
      @tt{ab+c}: multiple publishers with the same place of publication
      @subitem{@tt{$aNew York, NY :$bBarnes & Noble :$bSterling Publishing Co., Inc.,$c2012.} [@hyperlink["https://catalog.loc.gov/vwebv/staffView?bibId=17618487" "source"]]}
    }
    @item{
      @tt{(ab)+c}: multiple publications, each with different places and publishers
      @subitem{@tt{$aBoston :$bLee and Shepard, publishers ;$aNew York :$bLee, Shepard, and Dillingham,$c1872.} [@hyperlink["https://catalog.loc.gov/vwebv/staffView?bibId=19048248" "source"]]}
    }]

  Writing a regex to intelligently parse this information out of the string is
  a pain, but regexes are an already popular and well understood tool in the
  metadata community. Thus, @racket[marc-matcher] lets users specify regular
  expressions that match subgroups within the field they want to parse, and
  define variables they can use in their code containing the results of those
  matches, which allows more complex kinds of processing to be done with
  simpler code.

  @examples[#:eval marc-matcher-eval
    (define parse-264
      (marc-matcher ([#px"ab" #:as place-entity-groups]
                     [#px"c" #:as date])
                    (for/list ([group place-entity-groups])
                      (cons (marc-subfield-data date) (map marc-subfield-data group)))))
    (parse-264 "$aBoston :$bLee and Shepard, publishers ;$aNew York :$bLee, Shepard, and Dillingham,$c1872.")
  ]

  The first clause of the @racket[marc-matcher] expression is a list of variable
  definitions, similar to a parameter list for a lambda.
  For example, the clause @racket[[#px"ab" :as place-entity-groups]] defines a
  variable called @racket[place-entity-groups], which will be a list of all the
  groups (which are themselves lists of structs) consisting of a single
  subfield @racket[$a] followed by a single subfield @racket[$b].
  The second clause is the computation the user wishes to do with the values
  extracted from the field, and can refer to the variables defined in the first
  clause.

  Here is another example, using @hyperlink["https://www.loc.gov/marc/bibliographic/bd505.html" "table of contents"]
  data [@hyperlink["https://catalog.loc.gov/vwebv/staffView?bibId=17682122" "source"]].

  @examples[#:eval marc-matcher-eval
    (define matcher
      (marc-matcher ([#px"tr?" #:as title-info-groups])
        (for ([group title-info-groups])
          (define title (first (map marc-subfield-data
                                    (filter (λ (sf) (equal? "t" (marc-subfield-subtag sf))) group))))
          (define authors (map marc-subfield-data
                               (filter (λ (sf) (equal? "r" (marc-subfield-subtag sf))) group)))
          (printf "Title: ~a~a~n~n"
                  (string-trim title #px"( /\\s*)|( --\\s*)|\\.")
                  (if (empty? authors)
                      ""
                      (string-append "\nAuthor: "
                        (string-trim (first authors) #px"( /\\s*)|( --\\s*)|\\.")))))))
    (matcher
      (string-join '("$tCaveat Lector; or how I ransacked Wikipedias across the Multiverse soley "
                     "to amuse and edify readers -- $tMystery of the missing mothers / $rKristin King -- "
                     "$tSecrets of Flatland / $rAnne Toole -- $tSanyo TM-300 Home-Use Time Machine / "
                     "$rJeremy Sim -- $tElizabeth Burgoyne Corbett / $rL. Timmel Duchamp -- "
                     "$tBiographies.")
                   ""))]

  The macro definition parses the clauses for parameters and regexps, and then generates calls to run-time helper functions.

  @racketfile{marc-matcher.rkt}
}

@defstruct[marc-subfield ([subtag any/c] [data any/c]) #:transparent]

