#lang scheme

(require srfi/27)

(define (square x) (* x x))

(define (divides? a b)
	(= (remainder b a) 0))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
			((divides? test-divisor n) test-divisor)
			(else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
	(find-divisor n 2))

; above from 1.24

(define (expmod base exp m)
	(cond ((= exp 0) 1)
				((even? exp)
		 		 (cond 
		 		 	 ((= base 1) (remainder (square (expmod base (/ exp 2) m)) m))
		 		 	 ((= base (- m 1)) (remainder (square (expmod base (/ exp 2) m)) m))
		 		 	(else
		 		 		(cond 
		 		 		  ((= (remainder (square base) m) 1)
		 		 		  	0
		 		 		  )
		 		 		  (else 
		 		 		  	(remainder (square (expmod base (/ exp 2) m)) m)
		 		 		  )
		 		 		)
		 		  )
		 		 )
		 		)
				(else
		 		 (remainder (* base (expmod base (- exp 1) m)) m))))

;(define (expmod base exp m)
;	(cond ((= exp 0) 1)
;				((even? exp)
;		 		 (remainder (square (expmod base (/ exp 2) m)) m))
;				(else
;		 		 (remainder (* base (expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
  	(cond ((= (expmod a n n) 0) (display "Nontrivial Square Root Found ")))
  	(= (expmod a n n) a)
  )
  (try-it (+ 1 (random-integer (- n 1)))))

(define (miller-rabin-prime? n times)
	(cond ((= times 0) #t)
				((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
				(else #f)))

(miller-rabin-prime? 561 10000)
(miller-rabin-prime? 1105 10000)
(miller-rabin-prime? 1729 10000)
(miller-rabin-prime? 2465 10000)
(miller-rabin-prime? 2821 10000)
(miller-rabin-prime? 6601 10000)

(miller-rabin-prime? 8738 10000) ; not a prime

(miller-rabin-prime? 8737 10000) ; a real prime
(miller-rabin-prime? 7951 10000) ; a real prime
(miller-rabin-prime? 7993 10000) ; a real prime

