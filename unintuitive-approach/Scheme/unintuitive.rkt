#lang racket

(require "../../lib/Streams.rkt")

(provide natural-numbers)
(provide fibs)
(provide arithmetic-sequence)

(define natural-numbers
  (stream-cons 1 (stream-map (lambda (n) (+ n 1)) natural-numbers)))

(define zip-with
  (lambda (f xs ys)
    (stream-cons
     (f (car xs) (car ys))
     (zip-with f (stream-cdr xs) (stream-cdr ys)))))

(define fibs
  (stream-cons 1 (stream-cons 1 (zip-with + fibs (stream-cdr fibs)))))

(define arithmetic-sequence
  (lambda (a0 p)
    (stream-cons a0 (stream-map (lambda (n) (+ n p)) (arithmetic-sequence a0 p)))))