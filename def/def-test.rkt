#lang racket/base

;; NOTE: every `#:test` clause expands to a `module+ test`

(require syntax-parse-example/def/def)

(module+ test
  (require (only-in rackunit check-exn)))

(def (plus1 (x : integer?))
  "Add 1 to the given integer"
  #:test [
    ((plus1 1) ==> 2)
    ((plus1 2) ==> 3)]
  (+ x 1))

(module+ test
  (check-exn #rx"plus1: contract violation.*expected: integer?"
    (λ () (plus1 'a))))

(def (times3 y)
  "Multiply given number by 3"
  #:test [
    ((times3 3) ==> 9) ]
  (+ y y y))

(def (gcd (x : integer?) (y : integer?))
  "greatest common divisor"
  #:pre [
    (>= "First argument must be greater-or-equal than second")]
  #:test [
    ((gcd 10 3) ==> 1)
    ((gcd 12 3) ==> 3)]
  (cond
   [(zero? y) x]
   [else (gcd y (- x (* y (quotient x y))))]))

(module+ test
  (check-exn #rx"must be greater"
    (λ () (gcd 1 4))))

(def (zardoz x)
  "Return 42"
  #:test [
    ((zardoz 0) ==> 42)
    ((zardoz "asdf") ==> 42)]
  #:post [
    ((λ (r) (equal? r 42)) "should always return 42")]
  42)

(def (hello)
  "prints 'hello'"
  #:test [
    ((hello) ==> (void) #:stdout "hello\n")]
  (printf "hello\n"))
