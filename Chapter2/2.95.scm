#lang scheme

(require srfi/1)
(include "./algebra.scm")

(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))

(define q1 (mul p1 p2))
(print "q1        : ") (print q1) (newline)
(define q2 (mul p1 p3))
(print "q2        : ") (print q2) (newline)

(print "p1        : ") (print p1) (newline)
(define gcd-q1-q2 (apply-generic 'gcd q1 q2))
(print "gcd-q1-q2 : ") (print gcd-q1-q2) (newline)


