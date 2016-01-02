#lang racket

(provide parse-lisp)

(define (parse-lisp str)
  (parenthesize (tokenize str)))

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

;; everything below here is for parenthisizing
(define (ensure-list l)
  (if (list? l)
      l
      (list l)))

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
      (let ([tok (car toks)]
            [rest (cdr toks)]
            [lout (ensure-list out)])
        (match tok
          ["(" (append lout (p-help rest '() (add1 level)))]
          [")" (append (wrap-subterm lout level)
                       (p-help rest '() (sub1 level)))]
          [_ (p-help rest (append lout (list (classify tok))) level)]))]))
  (p-help toks '() -1))
