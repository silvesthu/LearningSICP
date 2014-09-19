#lang scheme

(include "interval.scm")

(define (mul-interval-ben x y)
	(let 	((lx (lower-bound x))
			 (ux (upper-bound x))
			 (ly (lower-bound y))
			 (uy (upper-bound y)))
		(display "")
		(cond 
			((and (> lx 0) (> ly 0)) (make-interval (* lx ly) (* ux uy)))
			((and (> lx 0) (> ly 0)) (make-interval (* lx ly) (* ux uy)))
		)
	)
)