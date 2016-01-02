#lang racket

(provide eval-lisp)

(require "lisp-parse.rkt")

(define repl-env (make-hash))

(define (repl)
  (display "slur-repl> ")
  (let ([line (read-line)])
    (match line
      [#f (exit)]
      [_ (displayln (eval-lisp (parse-lisp line) repl-env))]))
  (repl))

(define (eval-lisp ast env)
  ast)

(module+ main
  (repl))
