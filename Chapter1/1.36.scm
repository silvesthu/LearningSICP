#lang scheme

(define tolerance 0.00001)

(define index 0)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(set! index (+ 1 index))
			(display index) (display " guess = ") (display guess) (newline)
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess))

(define (average a b) (/ (+ a b) 2))

; 35 times
(fixed-point
	(lambda (y) 
		(/ (log 1000.0) (log y))
	)
	1.5
)

(set! index 0)

; 11 times
(fixed-point
	(lambda (y) 
		(average y (/ (log 1000.0) (log y)))
	)
	1.5
)