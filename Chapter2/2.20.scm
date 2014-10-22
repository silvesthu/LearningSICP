#lang scheme

(include "map.scm")

(filter (lambda (x) (= (remainder x 2) 0)) (list 1 2 3 4 5))
(filter (lambda (x) (= (remainder x 2) 1)) (list 1 2 3 4 5))

(define (same-parity x . y)
	(filter 
		(lambda (i) (= (remainder i 2) (remainder x 2)))
		(cons x y)
	)
)

(same-parity 1 2 3 4 5 6 7)
(same-parity 0 1 2 3 4 5 6 7)