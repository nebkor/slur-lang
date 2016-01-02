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

(define (ensure-list l)
  (if (list? l)
      l
      (list l)))

(define (atom? e)
  (not (list? e)))

(define (p-append head tail)
  (let ([lhead (ensure-list head)])
    (cond
     [(null? head) tail]
     [(null? tail) lhead]
     [else (append lhead (list tail))])))

(define (parenthesize toks)
  (define (p-help toks out)
    (displayln out)
    (cond
     ;[(atom? toks) (p-append out toks)]
     [(null? toks) out]
     [else
      (let* ([tok (car toks)]
             [rest (cdr toks)])
        (match tok
          ["(" (append (ensure-list out) (p-help rest '()))]
          [")" (append (list out) (p-help rest '()))]
          [_ (p-help rest (p-append out (classify tok)))]))]))
  (p-help toks '()))

(begin
  (define toks (tokenize "(a b c d (1 2 3 4) a1 (b1 c1) d1)"))
  (define test (parenthesize toks))
  (displayln test))
