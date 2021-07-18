#lang racket/base

(module test-success racket/base
  (require rackunit
           syntax/macro-testing
           syntax-parse-example/except-in-quiet/except-in-quiet)
  (require (except-in-quiet racket/list second))
  (check-not-exn (λ () (first '(1 2 3))))
  (check-exn exn:fail:syntax?
             (λ () (convert-compile-time-error (second '(1 2 3))))))
(require 'test-success)

(module test-failure racket/base
  (require rackunit
           syntax/macro-testing
           syntax-parse-example/except-in-quiet/except-in-quiet)
  (require (except-in-quiet racket/list snd))
  (check-exn exn:fail:syntax?
             (λ () (convert-compile-time-error snd))))
(require 'test-failure)

