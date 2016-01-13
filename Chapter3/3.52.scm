#lang racket
(require racket/stream)

(define sum 0)

(define (accum x)
	(display "accum ") (display x) (newline)
	(set! sum (+ x sum))
	sum
)

(define seq (stream-map accum (in-range 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

(stream->list seq)
(stream->list y)
(stream-ref y 7) ; 136

(stream->list z) ; '(10 15 45 55 105 120 190)

; if no memo-proc....
; result will changed since accum will be called multiple times