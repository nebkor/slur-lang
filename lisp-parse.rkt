#lang racket

(provide parse-lisp)

(define (parse-lisp str)
  str)

(define (tokenize str)
  (string-split
   (string-trim
    (let* ([noqm (string-replace str "'(" "(quote ")]
           [spaced-open (string-replace noqm "(" " ( ")])
      (string-replace spaced-open ")" " ) ")))))

(define (string-first s)
  (string-ref s 0))

(define (string-last s)
  (string-ref s (sub1 (string-length s))))

(define (pstring? s)
  (let ([s0 (string-first s)]
        [s1 (string-last s)])
    (and (eqv? #\" s0) (eqv? s0 s1))))

(define (classify tok)
  (let ([num (string->number tok)])
    (cond
     [(or num (pstring? tok)) (cons 'literal tok)]
     [else (cons 'identifier tok)])))

(define (ensure-list l)
  (if (list? l)
      l
      (list l)))

(define (p-append head tail)
  (let ([lhead (ensure-list head)])
    (cond
     [(null? head) tail]
     [(null? tail) lhead]
     [else (append lhead (list tail))])))

(define (wrap-subterm sterm level)
  (cond
   [(< level 1) sterm]
   [(null? sterm) sterm]
   [else
    (for/fold ([out sterm])
        ([l level])
      (list out))]))

(define (parenthesize toks)
  (define (p-help toks out level)
    (cond
     [(null? toks) out]
     [else
      (let* ([tok (car toks)]
             [rest (cdr toks)])
        (match tok
          ["(" (append (ensure-list out) (p-help rest '() (add1 level)))]
          [")" (if (not (null? out))
                   (append (wrap-subterm (ensure-list out) level)
                           (p-help rest '() (sub1 level)))
                   (append out (p-help rest '() (sub1 level))))]
          [_ (p-help rest (p-append out (classify tok)) level)]))]))
  (p-help toks '() -1))

(begin
  (define toks (tokenize "(a b c d (((1 2 3 4))) a1 a2 (b1 c1) d1 ((e1)) e2 e3 (e4 e5)))"))
  (define test (parenthesize toks)))
