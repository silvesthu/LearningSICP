#lang scheme

(define (square x) (* x x))

(define (even? n)
	(= (remainder n 2) 0))

(define (fast-expt b n)
	(cond ((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

(define (expmod-direct base exp m)
	(remainder (fast-expt base exp) m))

(define (expmod base exp m)
	(cond ((= exp 0) 1)
				((even? exp)
		 		 (remainder (square (expmod base (/ exp 2) m)) m))
				(else
		 		 (remainder (* base (expmod base (- exp 1) m)) m))))

; from 1.24

; -----------------

(define (test-carmichael-iter a n)
	(cond ((= a n) #t)
			(else 
				(cond 
					((= (expmod a n n) (remainder a n)) (test-carmichael-iter (+ a 1) n))
					(else #f)
				)
			)
	)
)

(define (test-carmichael n)
	(test-carmichael-iter 2 n))

(test-carmichael 561)
(test-carmichael 1105)
(test-carmichael 1729)
(test-carmichael 2465)
(test-carmichael 2821)
(test-carmichael 6601)

(test-carmichael 8737) ; a real prime
(test-carmichael 8738) ; not a prime
