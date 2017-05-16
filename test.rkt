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

    (define (filter/rx rx v*)
      (filter (λ (v) (regexp-match? rx v)) v*))

    (for ((d (in-glob "*/")))
      (cond
       [(compiled-dir? d)
        (void)]
       [(lang-dir? d)
        (define lang-file* (glob (build-path d "*.*")))
        (check-true (and (= 1 (length lang-file*))
                         (equal? (path->string (file-name-from-path (car lang-file*)))
                                 "reader.rkt")))]
       [else
        (define file* (for/list ([ps (in-glob (build-path d "*.*"))])
                        (path->string (file-name-from-path ps))))
        (define scrbl*
          (let ([f* (filter/rx #rx"\\.scrbl$" file*)])
            (check-equal? (length f*) 1 (format "directory '~a' contains multiple Scribble files" d))
            f*))
        (define test*
          (let ([f* (filter/rx #rx"-test\\.rkt$" file*)])
            (check-equal? (length f*) 1 (format "directory '~a' contains multiple '-test.rkt' files" d))
            f*))
        (define scrbl-name (cadr (regexp-match #rx"^([^.]*)\\.scrbl$" (car scrbl*))))
        (define test-name (cadr (regexp-match #rx"^([^.]*)-test\\.rkt$" (car test*))))
        (check-equal? scrbl-name test-name (format "directory '~a' has '.scrbl' and '-test.rkt' files with different prefixes" d))
        (define rkt-name (path-add-extension scrbl-name #".rkt"))
        (check-true (file-exists? (build-path d rkt-name)))
        (void)]))
  )
)



