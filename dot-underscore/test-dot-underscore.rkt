#lang racket
(require (for-syntax syntax/parse racket/base racket/syntax))
(require racket/class
         "dot-underscore.rkt")

;;;
;;; Transparent Objects
;;; 

; Make objects transparent.
; Note: (inspect (make-inspector)) is needed in the class definition.
(define Object
  (class* object% (printable<%>)
    (super-new)
    (inspect (make-inspector))
    (define/public (custom-print     port qq-depth) (do-print this print   port))
    (define/public (custom-display   port)          (do-print this display port))
    (define/public (custom-write     port)          (do-print this write   port))

    (define (do-print object out port)
      (define-values (class skipped?) (object-info this))
      (define-values (class-name field-cnt field-name-list field-accessor
                                 field-mutator super-class skipped?-)
        (class-info class))
      (display "<object:"                  port)
      (display (symbol->string class-name) port)
      (for ([field field-name-list])
        (display " ("                         port)
        (display (symbol->string field)       port)
        (display " "                          port)
        (out (dynamic-get-field field object) port)
        (display ")"                          port))
      (display ">"                         port))))

(define-syntax (Class stx)
  (syntax-parse stx 
    [(_Class name:id super-class:id . body)
     (syntax/loc stx       
       (define name
         (class* super-class ()           
           (inspect (make-inspector)) ; needed in order to support printing
           . body)))]))

;;;
;;; Test Dot Notation for Objects
;;;

(Class Horse Object
  (init-field breed)
  (field [legs 4])
  (define/public (run name)
    (displayln (string-append name " is running.")))
  (super-new))

(define bella (new Horse [breed "Danish Warmblood"]))

; How many legs does Bella have?
bella.legs ; not cached
bella.legs ; cached
bella.legs ; cached

; The owner buys a new horse.
(define lily  (new Horse [breed "Arabian"]))
; How many legs does Lily have?
lily.legs ; not cached
lily.legs ; cached
lily.legs ; cached

; The horses in the stable:
(define stable (vector bella lily))

; The number of horses in the stable.
stable.length ; 2

; The breeds of the horses.
(for/list ([horse stable]) horse.breed)
(for/list ([i stable.length]) stable_i.breed)


; The breeds of Bella and Lily?
bella.breed    ; Danish Warmblood
lily.breed     ; Arabian

; Bella has an accident

(:= bella.legs 3)
bella.legs      ; 3

; The owner has Bella put down. Buys Lily and renames Lily to Bella.
(set! bella lily)
; this tests that the cache doesn't get stuck on the old horse
bella.legs      ; 4
bella.breed     ; Arabian


; Methods invocation
(bella.run "Bella")



;;;
;;; Properties of vectors
;;;

(define fibs (vector 42 1 2 3 5 8 13 32))
(:= fibs_0 1)
fibs_0       ; 1

(define i 4)
fibs_4       ; 5
fibs_i       ; 5
(fibs.ref 4) ; 5

(define lucas (vector 1 3 4 7 11 18 29))
lucas_4 ; 11
lucas_i ; 11

(define tables (vector fibs lucas))
tables_0_4  ; 5
tables_1_4  ; 11

(= fibs.length (vector-length fibs))

fibs


