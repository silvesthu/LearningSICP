#lang scheme

; whether the program has to deal with large int

; from 1.16

(define (square x) (* x x))

(define (even? n)
	(= (remainder n 2) 0))

(define (fast-expt b n)
	(cond ((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

(define (expmod-direct base exp m)
	(remainder (fast-expt base exp) m))

; from 1.24

(define (expmod base exp m)
	(cond ((= exp 0) 1)
				((even? exp)
		 		 (remainder (square (expmod base (/ exp 2) m)) m))
				(else
		 		 (remainder (* base (expmod base (- exp 1) m)) m))))

(time (expmod-direct 123321 1000000 123)) ; 1549
(time (expmod 123321 1000000 123)) ; 0

; slow down to deal with large int