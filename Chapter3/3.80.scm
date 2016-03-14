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


(define (RLC R C L dt)
	(define (RLC-gen i0 v0)
		(define v (integral (delay dv) v0 dt))
		(define i (integral (delay di) i0 dt))
		(define dv (stream-scale i (/ -1 C)))
		(define di (stream-add (stream-scale v (/ 1 L)) (stream-scale i (/ (- R) L))))
		(cons v i)
	)
	RLC-gen
)

(define circuit (RLC 1 0.2 1 0.1))
(stream->list (stream-map (lambda (x) (stream-ref (car (circuit 0 10)) x)) (range 10)))
(stream->list (stream-map (lambda (x) (stream-ref (cdr (circuit 0 10)) x)) (range 10)))
