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

(define (monte-carlo experiment-stream passed failed)
	(define (next passed failed)
		(stream-cons
			(/ passed (+ passed failed))
			(monte-carlo
				(stream-rest experiment-stream) passed failed)))
	(if (stream-first experiment-stream)
		(next (+ passed 1) failed)
		(next passed (+ failed 1)))
)

(define (random-in-range low high)
	(let ((range (- high low)))
		(+ low (* range (random))))
)

(define (square x) (* x x))
(define (check) 
	(define diff (+ (square (- (random-in-range 2 8) 5)) (square (- (random-in-range 4 10) 7))))
	;(display diff) (newline)
	(<= diff (square 3))
)
(define (make-int-stream)
	(stream-cons 
		(check)
		(make-int-stream)
	)
)

(define int-stream (make-int-stream))
(define pi
	(stream-map (lambda (p) (* p 4.0))
		(monte-carlo int-stream 0 0))
)

(newline)
(stream-ref pi 100000)