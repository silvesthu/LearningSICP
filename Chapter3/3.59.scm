#lang racket
(require racket/stream)

(define (integrate-series stream)
	(define (integrate-series-inner stream-inner index)
		(if (stream-empty? stream-inner)
			stream-inner
			(stream-cons (* (stream-first stream-inner) (/ 1 index)) (integrate-series-inner (stream-rest stream-inner) (+ index 1)))
		)
	)
	(integrate-series-inner stream 1)
)

(stream->list (stream 1 5 7))
(stream->list (integrate-series (stream 1 5 7)))

; ---

(define exp-series
	(stream-cons 1 (integrate-series exp-series)))

(stream-ref exp-series 1)
(stream-ref exp-series 2) ; 2!
(stream-ref exp-series 3) ; 3!
(stream-ref exp-series 4) ; 4!

(define cosine-series
	(stream-cons 1 (stream-map (lambda (x) (- x)) (integrate-series sine-series))))

(define sine-series
	(stream-cons 0 (integrate-series cosine-series )))

(newline)

(stream-ref sine-series 1)
(stream-ref sine-series 2)
(stream-ref sine-series 3)
(stream-ref sine-series 4)
(stream-ref sine-series 5)
(stream-ref sine-series 6)
(stream-ref sine-series 7)
(stream-ref sine-series 8)






