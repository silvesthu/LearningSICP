#lang scheme

(provide prime?)
;(provide (rename-out [miller-rabin-prime? prime?]))

(require srfi/27)

(define (square x) (* x x))

(define (divides? a b)
	(= (remainder b a) 0))

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

(define (miller-rabin-test n)
  (define (try-it a)
  	(cond ((= (expmod a n n) 0)))
  	(= (expmod a n n) a)
  )
  (try-it (+ 1 (random-integer (- n 1)))))

(define (miller-rabin-prime? n times)
	(cond ((= times 0) #t)
				((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
				(else #f)))

(define (prime? n) (miller-rabin-prime? n 100))
