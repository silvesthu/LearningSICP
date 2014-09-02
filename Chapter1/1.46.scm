#lang scheme

(define (iterative-imporve good-enough-local? improve-guess-local)
	(lambda (guess)
		(if (good-enough-local? guess)
			guess
			((iterative-imporve good-enough-local? improve-guess-local) (improve-guess-local guess)) 
		)
	)
)

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (sqrt-iter guess x)
	(define (sqrt-iter-good-enough guess-good-enough) (< (abs (- (square guess-good-enough) x)) 0.001))
	(define (sqrt-iter-improve guess-improve) (average guess-improve (/ x guess-improve)))
	(
		(iterative-imporve sqrt-iter-good-enough sqrt-iter-improve)
		guess
	)
)

; test basic functions
; (sqrt-iter-good-enough 1.4 2)
; (sqrt-iter-improve 1 2)

(sqrt-iter 1.0 2) ; rewritten from version in 1.1.7

; ------------------------------

(define tolerance 0.00001)
(define (fixed-point f first-guess)
	(define (close-enough? guess-good-enough)
		(< (abs (- guess-good-enough (f guess-good-enough))) tolerance)
	)
	(define (next guess) (f guess))
	(
		(iterative-imporve 
			close-enough?
			next
		)
		first-guess
	)
)

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)