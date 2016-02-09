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

(define exp-series
	(stream-cons 1 (integrate-series exp-series)))

(define cosine-series
	(stream-cons 1 (stream-map (lambda (x) (- x)) (integrate-series sine-series))))

(define sine-series
	(stream-cons 0 (integrate-series cosine-series )))

(define (stream-add s1 s2)
	(stream-cons (+ (stream-first s1) (stream-first s2)) (stream-add (stream-rest s1) (stream-rest s2)))
)

(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream))

(define (mul-series s1 s2)
  (stream-cons
   (* (stream-first s1) (stream-first s2))
   (stream-add
    (stream-cons 0 (mul-series (stream-rest s1) (stream-rest s2))) ; need the cons to match order
    (stream-add
     (scale-stream (stream-rest s1) (stream-first s2))
     (scale-stream (stream-rest s2) (stream-first s1))))))

(define one-series (stream-add (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))

(stream-ref one-series 0) ; 1
(stream-ref one-series 1) ; 0
(stream-ref one-series 2) ; 0
(stream-ref one-series 3) ; 0
