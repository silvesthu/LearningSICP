#lang racket
(require racket/stream)

(define the-empty-stream (stream))

(define (integral delayed-integrand initial-value dt)
	(stream-cons initial-value
		(let 
			(
				(integrand (force delayed-integrand))
			)
			(if (stream-empty? integrand)
				(stream) ; empty stream
				(integral 
					(stream-rest integrand)
					(+ (* dt (stream-first integrand)) initial-value)
					dt))
			)			
	)
)

(define (solve f y0 dt)
	(define y (integral (delay dy) y0 dt))
	(define dy (stream-map f y))
	y
)

;(define (solve-2nd a b dt y0 dy0 y dy)
;	(define y (integral (delay dy) y0 dt))
;	(define dy (integral (delay ddy) dy0 dt))
;	(define ddy (+ (* a dy) (* b y)))
;	y
;)

(define (solve-2nd f dt y0 dy0)
	(define y (integral (delay dy) y0 dt))
	(define dy (integral (delay ddy) dy0 dt))
	(define ddy (f dy y))
	y
)

(define (stream-add s1 s2)
	(stream-cons (+ (stream-first s1) (stream-first s2)) (stream-add (stream-rest s1) (stream-rest s2)))
)

(define (stream-scale stream factor)
	(stream-map (lambda (x) (* x factor)) stream))

; from 3.6

;(require racket/base)
;(define (rand command . args)
;	(cond 
;		((eq? command 'generate) (random))
;		((eq? command 'reset) (lambda (x) (random-seed x)))
;	)
;)
;((rand 'reset) 117) ; set interal state (like a seed)
;(rand 'generate)
;(rand 'generate)

;((rand 'reset) 117) ; set interal state (like a seed)
;(rand 'generate)
;(rand 'generate)

; --------

(define (random-numbers signals)
	(if (stream-empty? signals)
		(stream)
		(let 
			(
				(signal (stream-first signals))
			)
			(cond 
				((eq? signal 'generate) (stream-cons (random) (random-numbers (stream-rest signals))))
				((integer? signal) (random-seed signal) (stream-cons (random) (random-numbers (stream-rest signals))))
				(else (error "Unknown signal"))
			)
		)
	)
)

(stream->list (random-numbers (stream 123 'generate 'generate 'generate 'generate 123 'generate 'generate 'generate)))

; better to use a non-globally random generator...