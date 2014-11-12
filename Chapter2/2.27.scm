#lang scheme

(define x (list (list 1 2) (list 3 4)))
x

(define (reverse data)
	(define (inner_reserve combined left)
		(display "combined = ")(display combined)
		(newline)
		(display "left     = ")(display left)
		(newline)
		(if (null? (cdr left))
			(cons (car left) combined)
			(inner_reserve (cons (car left) combined) (cdr left))
		)	
	)
	(if (null? data)
		data
		(inner_reserve null data)
	)
)

(reverse x)

(define (deep-reverse data)
	(define (inner_reserve combined left)
		;(display "combined = ")(display combined)
		;(newline)
		;(display "left     = ")(display left)
		;(newline)
		;(display "cdr left = ")(display (car left))
		;(newline)
		(if (null? (cdr left))
			(cons (car left) combined) ; no need to reverse null here
			(inner_reserve (cons (deep-reverse (car left)) combined) (cdr left))
		)	
	)
	(cond ((null? data) data)
		  ((pair? data) (inner_reserve null data))
		  (else data)
	)
)

(define z (list 2 3 (list (list 1 2 3) 5) (list 1 3) 3 5))
(display z)
(newline)
(deep-reverse z)