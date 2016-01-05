#lang scheme

(define (stream-map proc . argstreams)
	(if (null? (car argstreams))
		the-empty-stream
		(cons-stream ; create result streams
			(apply proc (map stream-car argstreams)) ; apply proc on first elements
			(apply stream-map
				(cons proc (map stream-cdr argstreams))) ; map on elements left
		)
	)
)