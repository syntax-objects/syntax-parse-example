#lang racket

(provide get-subfield-data
         simplify-groups
         (struct-out marc-subfield))

(struct marc-subfield (subtag data) #:transparent)

(define (parse-data marc-str [sep "$"])
  (for/list ([sf (string-split marc-str sep)])
    (marc-subfield (substring sf 0 1)
                   (substring sf 1 (string-length sf)))))

(define (filter-subfields groups subfields)
  (define (helper group remaining acc)
    (cond [(or (empty? group) (empty? remaining)) (values (reverse acc) remaining)]
          [(equal? (first group) (marc-subfield-subtag (first remaining)))
           (helper (rest group) (rest remaining) (cons (first remaining) acc))]
          [else (helper group (rest remaining) acc)]))
  (cond [(or (empty? groups) (empty? subfields)) '()]
        [else
         (let-values ([(result remaining) (helper (first groups) subfields '())])
           (cons result
                 (filter-subfields (rest groups) remaining)))]))

(define (str->strlist str)
  (map string (string->list str)))

(define (simplify-groups groups)
  (match groups
    [(list (list (marc-subfield t data)))
     (marc-subfield t data)]
    [_ groups]))

(define (get-subfield-data regexps subfield-str [sep "$"])
  (define subfields (parse-data subfield-str sep))
  (define subtags (string-join (map marc-subfield-subtag subfields) ""))
  (define subfield-groups (map (λ (re) (map str->strlist (regexp-match* re subtags))) regexps))
  (for/list ([group subfield-groups])
    (filter-subfields group subfields)))
