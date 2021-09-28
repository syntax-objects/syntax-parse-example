#lang racket
(module+ test
  (require rackunit syntax-parse-example/marc-matcher/marc-matcher)

  (test-case "parse-264"
    (define parse-264
      (marc-matcher ([#px"ab" #:as place-entity-groups]
                     [#px"c" #:as date])
                    (for/list ([group place-entity-groups])
                      (cons (marc-subfield-data date) (map marc-subfield-data group)))))
    (check-equal?
      (parse-264 "$aBoston :$bLee and Shepard, publishers ;$aNew York :$bLee, Shepard, and Dillingham,$c1872.")
      '(("1872." "Boston :" "Lee and Shepard, publishers ;")
        ("1872." "New York :" "Lee, Shepard, and Dillingham,"))))

  (test-case "table-of-contents"
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
    (define data
      '("$tCaveat Lector; or how I ransacked Wikipedias across the Multiverse soley "
        "to amuse and edify readers -- $tMystery of the missing mothers / $rKristin King -- "
        "$tSecrets of Flatland / $rAnne Toole -- $tSanyo TM-300 Home-Use Time Machine / "
        "$rJeremy Sim -- $tElizabeth Burgoyne Corbett / $rL. Timmel Duchamp -- "
        "$tBiographies."))
    (check-equal?
      (with-output-to-string
        (lambda () (matcher (string-join data ""))))
      (string-join
        '("Title: Caveat Lector; or how I ransacked Wikipedias across the Multiverse soley to amuse and edify readers"
          ""
          "Title: Mystery of the missing mothers"
          "Author: Kristin King"
          ""
          "Title: Secrets of Flatland"
          "Author: Anne Toole"
          ""
          "Title: Sanyo TM-300 Home-Use Time Machine"
          "Author: Jeremy Sim"
          ""
          "Title: Elizabeth Burgoyne Corbett"
          "Author: L. Timmel Duchamp"
          ""
          "Title: Biographies"
          ""
          "")
        "\n")))
)
