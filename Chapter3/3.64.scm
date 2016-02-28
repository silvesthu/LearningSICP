#lang racket
(require racket/stream)

(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
	(average guess (/ x guess)))

(define (sqrt-stream x)
	(define guesses
		(stream-cons 1.0
			(stream-map 
				(lambda (guess) 
					(sqrt-improve guess x))
				guesses)))
	guesses)

(define (stream-limit stream tolerance)
	(define (stream-limit-inner first stream tolerance)
		(if (< (abs (- first (stream-first stream))) tolerance)
			(stream-first stream)
			(stream-limit-inner (stream-first stream) (stream-rest stream) tolerance)
		)
	)
	(stream-limit-inner (stream-first stream) (stream-rest stream) tolerance)
)

(define (sqrt x tolerance)
	(stream-limit (sqrt-stream x) tolerance)
)

(sqrt 2 1)
(sqrt 2 0.1)
(sqrt 2 0.01)
(sqrt 2 0.0000000001)