#lang scheme

(define (compose f g) 
	(lambda (x) (f (g x)))
)

(define (sqaure x)
	(* x x)
)

(define (inc x) (+ x 1))

((compose sqaure inc) 6)