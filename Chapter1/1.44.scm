#lang scheme

(define (compose f g) 
	(lambda (x) (f (g x)))
)

(define (square x)
	(* x x)
)

(define (inc x) (+ x 1))

(define (repeated f n)
	(lambda (x)
		(if (= n 1) 
			(f x) ; last one
			((repeated f (- n 1)) (f x))
		)
	)
)

(define dx 0.000001)
(define (smooth f)
	(lambda (x)
		(/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)
	)
)

((smooth sin) 1)

(((repeated smooth 10) sin) 1)

(define (n-fold-smooth f n)
	(lambda (x) 
		(((repeated smooth n) f) x)
	)
)

((n-fold-smooth sin 10) 1)