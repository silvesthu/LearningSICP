#lang scheme

; use begin to combine multiple statements, and get return value from last one
;(display (begin 1 2))

(define (make-accumulator init)
	(define sum init)
	(lambda (addition) (set! sum (+ sum addition)) sum)
)

(define A (make-accumulator 5))

(A 10)

(A 10)