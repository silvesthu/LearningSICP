#lang scheme

; 2.17.scm

(define (last-pair data)
	(cond 
		((null? data) (list))
		((null? (cdr data)) (list (car data)))
		(else (last-pair (cdr data)))		
	)
)

(last-pair (list 23 72 149 34))