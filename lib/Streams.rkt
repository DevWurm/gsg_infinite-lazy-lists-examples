#lang racket

(provide stream-car)
(provide stream-cdr)
(provide stream-cons)
(provide stream?)
(provide stream-map)
(provide stream-filter)
(provide stream-show)
(provide stream-take)

(define stream-car car)

(define stream-cdr
  (lambda (stm)
    (force (cdr stm))))

(define-syntax stream-cons
  (syntax-rules ()
    ((_ head tail) (cons head (delay tail)))))

(define stream?
  (lambda (stm)
    (and (pair? stm) (promise? (cdr stm)))))

(define stream-map
  (lambda (proz stm)
    (stream-cons (proz (stream-car stm)) (stream-map proz (stream-cdr stm)))))

(define stream-filter
  (lambda (pred stm)
    (if (pred (stream-car stm))
        (stream-cons
         (stream-car stm)
         (stream-filter pred (stream-cdr stm)))
        (stream-filter pred (stream-cdr stm)))))

(define stream-show
  (lambda (stm n)
    (if (= n 0)
        (begin
          (display "...") (newline)
          'ok)
        (begin
          (display (stream-car stm)) (display ", ")
          (stream-show (stream-cdr stm) (- n 1))))))

(define stream-take
  (lambda (n stm)
    (cond
      ((= n 0) '())
      (else (cons
             (stream-car stm)
             (stream-take (- n 1)
                          (stream-cdr stm)))))))
