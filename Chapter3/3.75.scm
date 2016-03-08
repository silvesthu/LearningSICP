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

(define (sign-change-detector latter former)
	(cond 
		((and (< latter 0) (> former 0)) -1)
		((and (> latter 0) (< former 0)) 1)
		(else 0)
	)
)

(define (make-zero-crossings input-stream last-value last-avpt)
	(let ((avpt (/ (+ (stream-first input-stream) last-value) 2)))
		(stream-cons (sign-change-detector avpt last-avpt)
			(make-zero-crossings (stream-rest input-stream)
				(stream-first input-stream) avpt))
	)
)

(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))
(define zero-crossings (make-zero-crossings sense-data (stream-first sense-data) (stream-first sense-data)))

(map (lambda (x) (stream-ref zero-crossings x)) (stream->list (in-range 0 13))) ;