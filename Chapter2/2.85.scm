#lang scheme

(include "./algebra.scm")

(define tower '(number rational real complex))

(apply-generic 'simplifiable? 1)
(apply-generic 'simplifiable? (make-rational 4 1))
(apply-generic 'simplifiable? (make-real (/ 4 1)))
(apply-generic 'simplifiable? (make-complex-from-real-imag 4 0))

(define (to-simplest i)
	(if (apply-generic 'simplifiable? i)
		(to-simplest (apply-generic 'simplify i))
		i
	)
)

(to-simplest (make-complex-from-real-imag 4 0))
(to-simplest (make-complex-from-real-imag (/ 3 4) 0))
(to-simplest (make-complex-from-real-imag 1.2 0))
(to-simplest (make-complex-from-real-imag 4 3))

; only bi-directional conversion is considered here