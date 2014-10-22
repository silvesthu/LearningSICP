; 2.18.scm
#lang scheme

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

; can it be done without inner function ?
(reverse (list))
(reverse (list 23 45 67 89 0))
