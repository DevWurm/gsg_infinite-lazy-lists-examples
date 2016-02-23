#lang racket

(require "../../lib/Streams.rkt")

(provide natural-numbers)
(provide slow-fibs)
(provide fast-fibs)

(define natural-numbers
  (letrec ((calc-natural-numbers
            (lambda (n)
              (stream-cons n (calc-natural-numbers (+ n 1))))))
    (calc-natural-numbers 1)))

(define slow-fibs
  (letrec ((fib
            (lambda (n)
              (cond
                ((= n 1) 1)
                ((= n 2) 1)
                (else (+ (fib (- n 1)) (fib (- n 2))))))))
    (stream-map fib natural-numbers)))

(define fast-fibs
  (letrec ((calc-fibs
            (lambda (last)
              (let*
                  ((last1 (car last))
                   (last2 (cdr last))
                   (next (+ last1 last2)))
                (stream-cons next (calc-fibs (cons last2 next)))))))
    (append '(1 1) (calc-fibs (cons 1 1)))))

(define arithmetic-sequence
  (lambda (a0 p)
    (stream-cons a0 (arithmetic-sequence (+ a0 p) p))))