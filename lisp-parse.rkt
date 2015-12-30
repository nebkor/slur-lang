#lang racket

(provide parse-lisp)

(define reserved '(first rest car cdr list cons lambda cond if quote))

(define (parse-lisp str)
  str)

(define (tokenize str)
  (string-split (string-trim (string-replace
                               (string-replace str "(" " ( ")
                               ")" " ) "))))

(define (string-first s)
  (string-ref s 0))

(define (string-last s)
  (string-ref s (sub1 (string-length s))))

(define (classify tok)
  tok)

(define (parenthesize toks out)
  (cond
   [(null? toks) (reverse out)]
   [(eqv? #f out) (list (parenthesize toks '()))]
   [else
    (let* ([tok (car toks)]
           [rest (cdr toks)])
      (match tok
        ["(" (append out (parenthesize rest #f))]
        [")" (reverse out)]
        [a (parenthesize rest (cons (classify a) out))]))]))

(define toks (tokenize "(foo (bar (baz 2)))"))

(define test (parenthesize toks #f))

(displayln test)
