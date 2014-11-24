#lang scheme

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))
	)
)

(define (length sequence)
	(accumulate (lambda (x y) (+ 1 y)) 0 sequence)
)

(define (horner-eval x coefficient-sequence)
	(accumulate (lambda (this-coeff higher-terms)
					(+ (* higher-terms x) this-coeff)
				)
				0
				coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))

; for inner-computation 
; (the KEY is that computation occurs in a recursive way,
;  then find out y part for accumulation op -> result for each iteration)

; this-coeff iterates as (1,0,5,0,3,1) - reverse order of input

; higher-terms iterates as
; 0. 0 (intial)
; 1. an <- 0 * x + an
; 2. (an * x) + an-1 <- (an * x) + an-1
; 3. ...