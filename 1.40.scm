#lang scheme

(define (cubic a b c)
	(lambda (x) (+ (* x x x) (* a x x) (* b x) c))
)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess)
)

(define dx 0.00001)

(define (deriv g)
	(lambda (x)
		(/ (- (g (+ x dx)) (g x))
			dx)))

(define (newton-transform g)
	(lambda (x) 
		(- x (/ (g x) ((deriv g) x))))
)

(define (newtons-method g guess)
	(fixed-point (newton-transform g) guess)
)

(define a 2)
(define b 3)
(define c 4)

(newtons-method (cubic a b c) 1)

; x^3+2x^2+3x+4=0
; confirmed with wolframalpha