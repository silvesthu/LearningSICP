#lang racket
(require racket/stream)

(define (stream-add s1 s2)
	(stream-cons (+ (stream-first s1) (stream-first s2)) (stream-add (stream-rest s1) (stream-rest s2)))
)

(define (stream-scale stream factor)
	(stream-map (lambda (x) (* x factor)) stream))

(define (integral integrand initial-value dt)
	(define int
		(stream-cons initial-value
				(stream-add (stream-scale integrand dt)
				int))
	)
	int
)

(define (RC R C step)
	(define (RC-gen current-stream v0)
		(stream-add (stream-scale (integral current-stream v0 step) (/ 1 C)) (stream-scale current-stream R))
	)
	RC-gen
)

(define RC1 (RC 5 1 0.5))
(stream-ref (RC1 (stream 1 2 3 4 5) 0) 0)
(stream-ref (RC1 (stream 1 2 3 4 5) 0) 1)
(stream-ref (RC1 (stream 1 2 3 4 5) 0) 2)