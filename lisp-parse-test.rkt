#lang racket/base

(require "lisp-parse.rkt")

(define gnarly-sexp-string "(a b c d (((1 2 3 4))) a1 a2 (b1 c1) d1 ((e1)) e2 e3 (e4 e5)))")
(define gnarly-result '((identifier . "a")
                        (identifier . "b")
                        (identifier . "c")
                        (identifier . "d")
                        ((((literal . "1")
                           (literal . "2")
                           (literal . "3")
                           (literal . "4"))))
                        (identifier . "a1")
                        (identifier . "a2")
                        ((identifier . "b1") (identifier . "c1"))
                        (identifier . "d1")
                        (((identifier . "e1")))
                        (identifier . "e2")
                        (identifier . "e3")
                        ((identifier . "e4") (identifier . "e5"))))

(module+ test
  (require rackunit rackunit/text-ui)

  (define suite
    (test-suite
     "parse tests"
     (test-eqv? "single character" (parse-lisp "a") '((identifier . "a")))
     (test-eqv? "simple list" (parse-lisp "(a b)") '((identifier . "a") (identifier . "b")))
     (test-eqv? "gnarly-sexp" (parse-lisp gnarly-sexp-string) gnarly-result)
     (test-eqv? "literal quote" (parse-lisp "'(1 2 3)") '((identifier . "quote") (literal . "1") (literal . "2") (literal . "3")))))

  (run-tests suite))
