#lang typed/racket/base
(module+ test
  (require typed/rackunit
           (rename-in syntax-parse-example/struct-list/struct-list
                      [struct-list struct]))

  ;; Looks like a struct, but everything's made of lists
  (struct posn ([x : Real]
                [y : Real])
               #:type-name Posn)
  (struct block ([x : Real]
                 [y : Real]
                 [color : Symbol])
               #:type-name Block)
  (struct tetra ([center : Posn]
                 [blocks : (Listof Block)])
               #:type-name Tetra)

  (: posn=? (-> Posn Posn Boolean))
  (define (posn=? p1 p2)
    (and (= (posn-x p1) (posn-x p2))
         (= (posn-y p1) (posn-y p2))))

  (define origin (posn 0 0))
  (define line
    (tetra origin
           (for/list : (Listof Block)
                     ([y (in-list '(0 1 2 3))])
             (block 0 y 'blue))))

  (check-pred posn? origin)
  (check-pred tetra? line)
  (check-equal? (posn-x origin) (posn-y origin))
  (check-equal? 0 (apply + (map block-x (tetra-blocks line))))


  ;; Test internal representation
  (check-pred list? origin)
  (check-pred list? line)
  (check-equal? (cadr origin) (cadr origin))
  (check-equal? origin (tetra-center line))

)
