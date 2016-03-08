#lang racket
(require racket/stream)

(define (stream-add s1 s2)
	(stream-cons (+ (stream-first s1) (stream-first s2)) (stream-add (stream-rest s1) (stream-rest s2)))
)

(define (stream-scale stream factor)
	(stream-map (lambda (x) (* x factor)) stream))

(define (stream-multimap proc . argstreams)
	;(display (map stream->list argstreams)) (newline)
	;(display (map stream-first argstreams)) (newline)
	;(display (apply proc (map stream-first argstreams))) (newline)
	(if (null? (car argstreams))
		(stream)
		(stream-cons ; create result streams
			(apply proc (map stream-first argstreams)) ; apply proc on first elements
			(apply stream-multimap
				(cons proc (map stream-rest argstreams))) ; map on elements left
		)
	)
)

(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
(define (sign-change-detector latter former)
	(cond 
		((and (< latter 0) (> former 0)) -1)
		((and (> latter 0) (< former 0)) 1)
		(else 0)
	)
)

(define zero-crossings
	(stream-multimap sign-change-detector sense-data (stream-cons 0 sense-data)))

(map (lambda (x) (stream-ref zero-crossings x)) (stream->list (in-range 0 13))) ; mismatch of stream length will cause error on stream-first