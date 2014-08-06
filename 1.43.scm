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

((repeated square 2) 5)