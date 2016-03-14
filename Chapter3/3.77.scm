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

(stream->list (integral (stream 1 2 3) 1 1)) ; finite stream check

(stream-ref (solve (lambda (y) y) 1 0.001) 0)
(stream-ref (solve (lambda (y) y) 1 0.001) 1)
(stream-ref (solve (lambda (y) y) 1 0.001) 1000)