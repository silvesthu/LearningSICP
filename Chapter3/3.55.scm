#lang racket
(require racket/stream)

(define (add-streams s1 s2) (stream-cons (+ (stream-first s1) (stream-first s2)) (add-streams (stream-rest s1) (stream-rest s2))))
(define (mul-streams s1 s2) (stream-cons (* (stream-first s1) (stream-first s2)) (mul-streams (stream-rest s1) (stream-rest s2))))

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))


(define (partial-sums s)
	(define first-copies (stream-cons (stream-first s) first-copies))
	(stream-cons (stream-first s) (add-streams first-copies (partial-sums (stream-rest s))))
)

(stream-ref (partial-sums integers) 0)
(stream-ref (partial-sums integers) 1)
(stream-ref (partial-sums integers) 2)
(stream-ref (partial-sums integers) 3)
(stream-ref (partial-sums integers) 4)
(stream-ref (partial-sums integers) 5)
(stream-ref (partial-sums integers) 6)