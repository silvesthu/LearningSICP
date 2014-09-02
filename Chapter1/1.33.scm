#lang scheme

(require "./../common/prime.scm")

(define (filtered-accumulate combiner null-value term a next b predicate)
	(let ((value (term a)))
		(cond ((> a b)
						null-value)
					((predicate a)
						(combiner	value
							(filtered-accumulate combiner null-value term (next a) next b predicate)))
					(else	
				 		(filtered-accumulate combiner null-value term (next a) next b predicate))
		)
	)
)

(define (prime-square-sum a b)
	(filtered-accumulate
		+
		0
		(lambda (x) (* x x))
		a
		(lambda (x) (+ x 1))
		b
		prime?
	)
)

(prime-square-sum 2 4) 
; so should predicate be used on a or (term a) ?
; the name filter indicates it should be used on (term a)
; however the problem needs to be applied on a
; Or just put a and (term a) both in the parameter list of predicate ?

(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))))

(define (positive-less-n-prime n)
	(filtered-accumulate
		*
		1
		(lambda (x) x)
		1
		(lambda (x) (+ x 1))
		n
		(lambda (a) (= (gcd a n) 1))
	)
)

(positive-less-n-prime 5) ; 2 * 3 * 4
(positive-less-n-prime 6) ; 5

		
