#lang racket/base
;;;
;;; Exports
;;;

(provide
 :=
 (rename-out [top #%top])
 (rename-out [app #%app]))


;;;
;;; Imports
;;;

(require (for-syntax racket/base
                     syntax/parse
                     syntax/strip-context
                     racket/string
                     racket/syntax)
         racket/list
         racket/class
         racket/format)

;;;
;;; Syntax Utilities
;;; 

(begin-for-syntax

  (define (number-id->number id)
    (define x (string->number (symbol->string (syntax-e id))))
    (if (integer? x) x id))
  
  (define (stringify x #:mode [mode 'convert])
    (cond
      [(string? x)     x]
      [(char? x)       (string x)]
      [(symbol? x)     (symbol->string x)]
      [(number? x)     (number->string x)]      
      [(identifier? x) (symbol->string (syntax-e x))]
      [else            #f]))
  
  (define (id-contains? id needle)
    (string-contains? (stringify id) (stringify needle)))
  
  (define (index-of-char c str [start 0])
    (define n (string-length str))
    (and (< start n)
         (for/first ([i (in-range start n)]
                     #:when (char=? (string-ref str i) c))
           i)))
  
  (define (index-of-dot        str [start 0]) (index-of-char #\. str start))
  (define (index-of-underscore str [start 0]) (index-of-char #\_ str start))
  
  (define (index-of-dot/underscore str [start 0])
    (define i (index-of-dot        str start))
    (define j (index-of-underscore str start))
    (or (and i j (min i j))
        i
        j))
  
  (define (split-string-at-dot/underscore str [start 0])
    ; > (split-string-at-dot/underscore "foo.bar_baz.qux")
    ; '("foo" "." "bar" "_" "baz" "." "qux")
    (define i (index-of-dot/underscore str start))
    (cond
      [i (define pre  (substring str start i))
         (define sep  (string (string-ref str i)))
         (define post (split-string-at-dot/underscore str (+ i 1)))
         (list* pre sep post)]
      [else
       (list (substring str start (string-length str)))]))
  
  (define (split-id-at-dot/underscore id [context id] [prop #f])
    (define str (symbol->string (syntax-e id)))
    (define strs (split-string-at-dot/underscore str))
    (define srcloc id)
    (for/list ([str strs])
      (define sym (string->symbol str))
      (datum->syntax context sym srcloc prop)))
  

  (define (split-id id sep [context id] [prop #f])
    ; note: context is either #f or a syntax object
    ;       prop    is either #f or a syntax object
    (cond
      [(not (id-contains? id sep)) id]
      [else                        
       (define strs (string-split (stringify id) (stringify sep)))
       (define srcloc id)
       (for/list ([str strs])
         (define sym (string->symbol str))
         (datum->syntax context sym srcloc prop))])))


;;;
;;; #%top - Top-level variable references
;;; 

; SYNTAX  (top . id)

;   Like #%top, but additionally:
;     - identifiers containing a dot is rewritten to use dot-field,
;       enabling  (#%app foo.bar 1 2) -> (send foo bar 1 2 )
;       and       foo.bar             -> (get-field foo bar)

; Note: top is exported as #%top

(define-syntax (top stx)
  (syntax-parse stx
    [(_top . top-id:id)
     (cond
       [(or (id-contains? #'top-id ".") (id-contains? #'top-id "_"))
        (define ids (map number-id->number (split-id-at-dot/underscore #'top-id)))       
        (with-syntax ([(id ...) ids])
          (syntax/loc stx (dot/underscore id ...)))]
       [else
        #'(#%top . top-id)])]
    [(_top . something)
     ; get the original error when top is misused
     #'(#%top . something)]))

;;;
;;; #%app - Procedure application
;;; 

; SYNTAX  (app proc-expr arg ...)

;   Like #%app, but additionally:
;     - a call to  o.m  where o is an object and m is a method name,
;       is rewritten to a method call
;     - a call to  f, where the identifier f contains no dots,
;       is a normal call.

; Note: app is exported as #%top

(define-syntax (app stx)
  (syntax-parse stx
    ; calls to a   o.m is a method call
    ; calls to an  f   is a normal call
    [(app proc-id:id . args)
     (define str (symbol->string (syntax-e #'proc-id)))
     (cond
       [(string-contains? str ".")
        (define (transfer-scope id) (datum->syntax #'proc-id id))
        (define ids (map transfer-scope (map string->symbol (string-split str "."))))
        (with-syntax ([(id ...) (map transfer-scope ids)])
          #'(app-dot-method (id ...) . args))]
       [else
        #'(#%app proc-id . args)])]
    [(app . more)
     (syntax/loc stx
       (#%app . more))]))

;;;
;;; := - Assignment
;;; 

(define-syntax (:= stx)
  (syntax-parse stx
    [(_ x:id e)     
     (cond
       [(or (id-contains? #'x ".") (id-contains? #'x "_"))
        (define ids (map number-id->number (split-id-at-dot/underscore #'x)))
        (with-syntax ([(id ...) ids])
          (syntax/loc stx
            (assign-dot/underscore id ... e)))]
       [else
        (syntax/loc stx
          (set! x e))])]
    [(_ v:id e1 e2)
     (syntax/loc stx
       (let ()
         (when (vector? v)
           (define i e1)
           (when (and (integer? i) (not (negative? i)))
             (vector-set! v i e2)))))]))


;;;
;;; Dot Fields and Methods
;;;

; For a struct Circle defined as:
;     (struct Circle (x y r))
; we would like to write
;     (define c (Circle 10 20 30))
;     (draw-circle c.x c.y c.r)
; in order to draw the circle. Here draw-circle is some
; function that draws a circle.

; The value stored in c is a Circle struct, so we need
; associate Circle structs with a set of fields.

; (define-fields Circle Circle? (x y r))

; We can now write:
;    (define (draw-circle c)
;      (circle c.x c.y c.r))
; where circle is a primitive that draws a circle.


; If we want a method draw that draws a given circle in a certain color,
; we use  define-method:

; (define-method Circle Circle? (draw color)
;   (stroke color)
;   (circle (Circle-x this) (Circle-y this))

; Here  stroke  is a primitive that sets the pen color.


; The general syntax is:

;; (define-method name predicate
;;   (method-name . formals) . body)



;;;
;;; Implementation
;;;

; Each "class" e.g. struct or vector is identified by an unique symbol,
; called the class symbol. 
; For (define-fields Circle Circle? (x y r)) the class symbol will be Circle.
; Each class has a list of fields, here the list of fields are x, y and z.

; Now at runtime  c.x  must called the accessor Circle-x,
; so we also need to store the accessor of each field.

; And later we want to support assignments of the form (:= c.x 10),
; so we also store mutators for the fields.

(struct info (accessor mutator))

; symbol -> (list (cons symbol info) ...)
(define class-symbol-to-fields+infos-ht     (make-hasheq))

(define (add-fields! class-symbol fields accessors mutators)
  ; todo: signal error, if class symbol is in use?
  (define fields+infos (map cons fields (map info accessors mutators)))
  (hash-set! class-symbol-to-fields+infos-ht class-symbol fields+infos))

(define (get-fields+infos class-symbol)
  (hash-ref class-symbol-to-fields+infos-ht class-symbol #f))

(define (find-info fields+infos field)
  (cond
    [(assq field fields+infos) => (λ (x) (cdr x))]
    [else                          #f]))

(define (find-accessor fields+infos field)
  (cond
    [(assq field fields+infos) => (λ (x) (info-accessor (cdr x)))]
    [else                          #f]))

(define (find-mutator fields+infos field)
  (cond
    [(assq field fields+infos) => (λ (x) (info-mutator (cdr x)))]
    [else                          #f]))


; In an expression like  c.x  the identifier c is bound to
; a value. To get from the value to the class symbol, we
; need some predicates.


(define class-symbol-to-predicate-ht     (make-hasheq)) ; symbol -> (value -> boolean)
(define predicate-to-class-symbol-assoc  '())           ; (list (cons predicate symbol) ...)


(define (add-predicate! class-symbol predicate)
  (hash-set! class-symbol-to-predicate-ht class-symbol predicate)
  (set! predicate-to-class-symbol-assoc
        (cons (cons predicate class-symbol)
              predicate-to-class-symbol-assoc)))

(define (find-class-symbol value)
  (let loop ([as predicate-to-class-symbol-assoc])
    (cond
      [(null? as)  #f]      ; the value has no associated class symbol
      [else        (define predicate+symbol (car as))
                   (define predicate        (car predicate+symbol))
                   (if (predicate value)
                       (cdr predicate+symbol)
                       (loop (cdr as)))])))

(define (find-class-symbol+predicate value)
  (let loop ([as predicate-to-class-symbol-assoc])
    (cond
      [(null? as)  (values #f #f)]  ; the value has no associated class symbol
      [else        (define predicate+symbol (car as))
                   (define predicate        (car predicate+symbol))
                   (if (predicate value)
                       (values (cdr predicate+symbol) predicate)
                       (loop (cdr as)))])))

; Finally we need address how  c.x  eventually evaluates as (Circle-x c).
; Since  c.x  isn't bound in our program, the expander expands
; c.x into (#%top c.x). In "main.rkt" we have defined a
; #%sketching-top, which will be used as #%top by programs
; written in the Sketching language.

; In #%sketching-top we detect that the unbound identifier contains
; a dot, and we can then expand it into:
;   (dot-field c x)
; The naive implementation of  dot-field will expand into:

;; (let ()
;;   (define class-symbol     (find-class-symbol c))
;;   (define fields+accessors (get-fields+accessors class-symbol))
;;   (define accessor         (find-accessor fields+accessors 'x))
;;   (accessor c))

; However, if c.x is being used in a loop where c runs through a series of circles,
; then it is a waste of time to search for the same class symbol and accessor
; each time. Instead we can cache the accessor (and a predicate) so we can reuse
; the next time. The predicate is used to check that we got a value of the
; same time as last - if not, then we need to look for the new class.

; Note: The first design used . for both field access and for vector/string indexing.
;       Expanding into  (if (object? obj) (get-field field obj) (vector-ref obj field))
;       leads to an unbounded reference (at compile time) when object.field is compiled,

;; Syntax classes for the separators.

(begin-for-syntax
  (define-syntax-class dot        #:datum-literals (|.|) (pattern |.|))
  (define-syntax-class underscore #:datum-literals (|_|) (pattern |_|))
  (define-syntax-class separator (pattern (~or _:dot _:underscore))))
  

; References to identifiers containing dots or underscores are handled by dot/underscore.
(define-syntax (dot/underscore stx)
  (syntax-parse stx
    [(_dot/underscore object:id)
     ; references to identifiers without dots are plain references
     #'object]
    [(_dot/underscore object:id sep:dot field:id)
     ; This stems from a reference of the kind  obj.field
     ; The field accessor is cached.
     (with-syntax
       ; locations to store the accessor and object predicate
       ([cached-accessor  (syntax-local-lift-expression #'#f)]  ; think: (define cached-accessor #f) 
        [cached-predicate (syntax-local-lift-expression #'#f)]) ;        is added at the top-level       
       #'(let ()            
           (define accessor
             ; We need to access the field of the object.
             ; If we referenced a field in the same object recently, we use the cached accessor.
             ; Otherwise, we need to find the access method and cache it first.
             (cond
               [(and cached-predicate (cached-predicate object))
                cached-accessor]
               [(object? object)
                ; If object is an instance of a class, we can use  get-field.
                (set! cached-accessor  (λ (obj) (get-field field obj)))
                (set! cached-predicate object?)
                cached-accessor]
               [else
                ; If the "object" is a vector, string or similar, we need to check whether
                ; added any properties (aka fake fields).
                (define-values (class-symbol predicate) (find-class-symbol+predicate object))
                (define fields+infos                    (get-fields+infos class-symbol))
                (define a                               (find-accessor fields+infos 'field))
                (set! cached-accessor  a)
                (set! cached-predicate predicate)
                (or a (raise-syntax-error 'dot/underscore "object does not have this field" #'stx))]))
           (accessor object)))]
    [(_dot/underscore object:id sep:underscore index)
     ; This stems from a reference of the kind  obj_index.
     ; If the object is a vector or a string, we can use vector-ref or string-ref to get the slot.
     ; The accessor is cached.
     (with-syntax
       ([cached-accessor  (syntax-local-lift-expression #'#f)]  ; think: (define cached-accessor #f) 
        [cached-predicate (syntax-local-lift-expression #'#f)]) ;        is added at the top-level
       #'(let ()            
           (define accessor
             (cond
               [(and cached-predicate (cached-predicate object))
                cached-accessor]
               [(vector? object)
                (set! cached-accessor  vector-ref)
                (set! cached-predicate vector?) 
                cached-accessor]
               [(string? object)
                (set! cached-accessor  string-ref)
                (set! cached-predicate string?) 
                cached-accessor]
               [else
                (set! cached-accessor  #f)
                (set! cached-predicate #f)
                (raise-syntax-error 'dot/underscore "value is not indexable with underscore" #'stx)]))
           (accessor object index)))]
    [(_dot/underscore object:id (~seq sep field-or-index) ... last-sep last-field-or-index)
     ; field and index references are left associative
     (syntax/loc stx
       (let ([t (dot/underscore object (~@ sep field-or-index) ...)])
         (dot/underscore t last-sep last-field-or-index)))]))


; Assignment where the "lvalue" is an identifier containing dots or underscores is handled by assign-dot/underscore.
(define-syntax (assign-dot/underscore stx)
  (syntax-parse stx
    [(_assign-dot/underscore object:id sep:dot field:id e:expr)
     ; This stems from an assignment of the kind (:= object.field e)
     ; The mutator is cached.
     (with-syntax ([cached-mutator   (syntax-local-lift-expression #'#f)]
                   [cached-predicate (syntax-local-lift-expression #'#f)])
       (syntax/loc stx
         (let ()
           (define mutator
             (cond
               [(and cached-predicate (cached-predicate object))
                cached-mutator]
               [(object? object)
                ; If object is an instance of a class, we can use  set-field!
                (set! cached-mutator  (λ (obj v) (set-field! field obj v)))
                (set! cached-predicate object?)
                cached-mutator]               
               [else
                ; If the "object" is a vector, string or similar, we need to check whether
                ; added any properties (aka fake fields).
                (define-values (class-symbol predicate) (find-class-symbol+predicate object))
                (define fields+infos                    (get-fields+infos class-symbol))
                (define m                               (find-mutator fields+infos 'field))
                (set! cached-mutator   m)
                (set! cached-predicate predicate)
                (unless m
                  (raise-syntax-error ':= "object does not have this field: ~a" (syntax-e #'field)))
                m]))
           (mutator object e))))]
    
    [(_assign-dot/underscore object:id sep:dot not-a-field e:expr)
     (raise-syntax-error ':= "field name (i.e. an identifier) expected after the dot"
                         stx)]
    
    [(_assign-dot/underscore object:id sep:underscore index:expr e:expr)
     ; This stems from an assignment of the kind (:= object_index e)
     ; where object is indexable (i.e. a vector or string).
     ; The mutator is cached.
     (with-syntax ([cached-mutator   (syntax-local-lift-expression #'#f)]
                   [cached-predicate (syntax-local-lift-expression #'#f)])
       (syntax/loc stx
         (let ()
           (define mutator
             (cond
               [(and cached-predicate (cached-predicate object))
                cached-mutator]
               [(vector? object)
                (set! cached-mutator  (λ (obj v) (vector-set! obj index v)))
                (set! cached-predicate vector?)
                cached-mutator]
               [(string? object)
                (set! cached-mutator  (λ (obj v) (string-set! obj index v)))
                (set! cached-predicate string?)
                cached-mutator]
               [else
                (raise-syntax-error ':= "underscore expects the object to be a vector or string" #'stx)]))
           (mutator object e))))]
    
    [(_assign-dot/underscore object:id (~seq sep:separator field-or-index) ... 
                             last-sep:separator last-field-or-index e:expr)
     (syntax/loc stx
       (let ([w e] [t (dot/underscore object (~@ sep field-or-index) ...)])
         (assign-dot/underscore t last-sep last-field-or-index w)))]))


;;;
;;; Defining class with fields
;;;

; We would like to write:
;     (declare-struct-fields Circle Circle? (x y r))
; to declare that the Circle "class" has fields x, y and r.

; The general form is:
;     (declare-struct-fields class-symbol predicate (field ...))

(define-syntax (declare-struct-fields stx)
  (syntax-parse stx
    [(_declare-struct-fields struct-name:id predicate:id (field:id ...))
     (with-syntax ([class-symbol #'struct-name]
                   [(accessor ...)
                    (for/list ([f (syntax->list #'(field ...))])
                      (format-id f "~a-~a" #'struct-name f))]
                   [(mutator ...)
                    (for/list ([f (syntax->list #'(field ...))])
                      (format-id f "set-~a-~a!" #'struct-name f))])
       (syntax/loc stx
         (begin
           (add-predicate! 'class-symbol predicate)
           (add-fields! 'class-symbol '(field ...)
                        (list accessor ...)
                        (list mutator  ...)))))]))

;;;
;;; Methods
;;;

; For a struct Circle defined as:

;     (struct Circle (x y r))

; we would like to write

;     (define c (Circle 10 20 30))
;     (define-method Circle (draw this color)
;        (stroke color)
;        (circle this.x this.y this.r))
;     (c.draw "red")

; in order to draw the circle.

; The method draw of the class Circle is called as (c.draw "red")
; and turns into (Circle-draw c "red").

(struct minfo (procedure)) ; "method info"

; symbol -> (list (cons symbol minfo) ...)
(define class-symbol-to-methods+minfos-ht (make-hasheq))

(define (add-method! class-symbol method procedure)  
  (define methods+minfos (hash-ref class-symbol-to-methods+minfos-ht class-symbol '()))  
  (define method+minfo   (cons method (minfo procedure)))
  (hash-set! class-symbol-to-methods+minfos-ht class-symbol (cons method+minfo methods+minfos)))

(define (get-methods+minfos class-symbol)
  (hash-ref class-symbol-to-methods+minfos-ht class-symbol #f))

(define (find-method-procedure methods+minfos method)
  (cond
    [(assq method methods+minfos) => (λ (x) (minfo-procedure (cdr x)))]
    [else                            #f]))


(define-syntax (app-dot-method stx)
  (syntax-parse stx
    [(_app-dot-method (object:id method:id) . args)
     (with-syntax
       ([cached-procedure (syntax-local-lift-expression #'#f)] ; think: (define cached-procedure #f) 
        [cached-predicate (syntax-local-lift-expression #'#f)] ;        is added at the top-level
        [stx stx])
       #'(let ()
           (define procedure
             (cond
               [(and cached-predicate (cached-predicate object)) cached-procedure]
               [(object? object) ; XXX
                (set! cached-predicate object?)
                (set! cached-procedure (λ (obj . as)
                                         (send/apply obj method as)))
                cached-procedure]
               [else
                (define-values (class-symbol predicate) (find-class-symbol+predicate object))
                (define methods+minfos                  (get-methods+minfos class-symbol))
                (define p                               (find-method-procedure methods+minfos 'method))
                (set! cached-procedure p)
                (set! cached-predicate predicate)
                (unless p
                  (raise-syntax-error 'app-dot-methods "object does not have this method" #'stx))
                p]))
           (procedure object . args)))]
    [(_app-dot-field (object:id field:id ... method:id) . args)
     (syntax/loc stx
       (let ([obj (dot-field object field ...)])
         (app-dot-method (obj method) . args)))]))


(define-syntax (define-method stx)
  (syntax-parse stx
    [(_define-method struct-name:id (method-name:id . formals) . more)
     (with-syntax ([struct-predicate (format-id stx "~a?"   #'struct-name)]
                   [struct-method    (format-id stx "~a-~a" #'struct-name #'method-name)]
                   [this             (format-id stx "this")])
       (syntax/loc stx
         (begin
           (define (struct-method this . formals) . more)
           (add-predicate! 'struct-name struct-predicate)
           (add-method! 'struct-name 'method-name struct-method))))]))


;;;
;;; Builtin "classes"
;;;

(add-predicate! 'vector vector?)
(add-predicate! 'list   list?)
(add-predicate! 'string string?)

(define vector-ref*
  ; this handles vectors, vectors of vectors, and vectors of vectors of vectors
  (case-lambda
    [(this i1)       (vector-ref this i1)]
    [(this i1 i2)    (vector-ref (vector-ref this i1) i2 )]
    [(this i1 i2 i3) (vector-ref (vector-ref (vector-ref this i1) i2) i3)]
    [else (error 'vector-ref* "only 3 indices supported")]))

;;; Builtin fields for builtin "classes"
(add-fields! 'vector '(ref length x y z)             
             (list vector-ref*
                   vector-length
                   (λ (v) (vector-ref v 0))
                   (λ (v) (vector-ref v 1))
                   (λ (v) (vector-ref v 2)))
             (list #f
                   #f
                   (λ (v e) (vector-set! v 0 e))
                   (λ (v e) (vector-set! v 1 e))
                   (λ (v e) (vector-set! v 2 e))))

(add-fields! 'list '(x y z)
             (list first second third)
             (list #f    #f     #f))


(add-method! 'vector 'length vector-length)
(add-method! 'vector 'ref    vector-ref*)
(add-method! 'vector 'list   vector->list)
(add-method! 'vector 'fill!  vector-fill!)
(add-method! 'vector 'values vector->values)

(add-method! 'list 'length  length)
(add-method! 'list 'ref     list-ref)
(add-method! 'list 'vector  list->vector)

(add-method! 'string 'length  string-length)
(add-method! 'string 'ref     string-ref)
(add-method! 'string 'list    string->list)


