#lang racket/base

;; Whole-package tests

(module+ test
  (require rackunit file/glob racket/path)

  (test-case "directory-structure"
    ;; All directories should contain at least 3 files:
    ;; - NAME.rkt
    ;; - NAME.scrbl
    ;; - NAME-test.rkt
    ;; Except for the `lang/` directory, which should just contain `reader.rkt`

    (define ((make-directory-name=? target) dir)
      (define-values [_base name _dir?] (split-path dir))
      (string=? (path->string name) target))

    (define lang-dir? (make-directory-name=? "lang"))
    (define compiled-dir? (make-directory-name=? "compiled"))
    (define doc-dir? (make-directory-name=? "doc"))

    (define (filter/rx rx v*)
      (filter (λ (v) (regexp-match? rx v)) v*))

    (define (<=1 n)
      (<= 1 n))

    (for ((d (in-glob "*/")))
      (cond
       [(compiled-dir? d)
        (void)]
       [(lang-dir? d)
        (define lang-file* (glob (build-path d "*.*")))
        (check-true (and (= 1 (length lang-file*))
                         (equal? (path->string (file-name-from-path (car lang-file*)))
                                 "reader.rkt")))]
       [(doc-dir? d)
        (void)]
       [else
        (define file* (for/list ([ps (in-glob (build-path d "*.*"))])
                        (path->string (file-name-from-path ps))))
        (define scrbl*
          (let ([f* (filter/rx #rx"\\.scrbl$" file*)])
            (check-pred <=1 (length f*) (format "directory '~a' must contain at least one Scribble file" d))
            f*))
        (define test*
          (let ([f* (filter/rx #rx"-test\\.rkt$" file*)])
            (check-pred <=1 (length f*) (format "directory '~a' must contain at least one '-test.rkt' file" d))
            f*))
        (define scrbl-name (cadr (regexp-match #rx"^([^.]*)\\.scrbl$" (car scrbl*))))
        (define test-name (cadr (regexp-match #rx"^([^.]*)-test\\.rkt$" (car test*))))
        (check-equal? scrbl-name test-name (format "directory '~a' has '.scrbl' and '-test.rkt' files with different prefixes" d))
        (define rkt-name (path-add-extension scrbl-name #".rkt"))
        (check-true (file-exists? (build-path d rkt-name)))
        (void)]))
  )
)



