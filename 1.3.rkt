#lang scheme

(define (sqrsum2of3 x y z) 
		(cond	( (< x y)
					(cond (	(< x z) (+ (* z z) (* y y)))
						  ( else (+ (* x x) (* y y)))
					)
				)
				( else 
					(cond (	(< y z) (+ (* z z) (* x x)))
						  ( else (+ (* y y) (* x x)))
					)
				)
		)
)

(sqrsum2of3 5 6 3)