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