#lang scheme

(include "interval.scm")

(define (mul-interval-ben x y)
	(let 	((lx (lower-bound x))
			 (ux (upper-bound x))
			 (ly (lower-bound y))
			 (uy (upper-bound y)))
		(cond 
			((and (> lx 0) (> ux 0) (> ly 0) (> uy 0)) (make-interval (* lx ly) (* ux uy))) ; ++
			((and (> lx 0) (> ux 0) (< ly 0) (< uy 0)) (make-interval (* ux uy) (* lx ly))) ; +-
			((and (< lx 0) (< ux 0) (> ly 0) (> uy 0)) (make-interval (* ux uy) (* lx ly))) ; -+
			((and (< lx 0) (< ux 0) (< ly 0) (< uy 0)) (make-interval (* lx ly) (* ux uy))) ; --

			((and (< lx 0) (> ux 0) (> ly 0) (> uy 0)) (make-interval (* lx uy) (* ux uy))) ; ?+
			((and (> lx 0) (> ux 0) (< ly 0) (> uy 0)) (make-interval (* ly ux) (* ux uy))) ; +?

			((and (< lx 0) (> ux 0) (< ly 0) (< uy 0)) (make-interval (* ux uy) (* lx uy))) ; ?-
			((and (< lx 0) (< ux 0) (< ly 0) (> uy 0)) (make-interval (* ux uy) (* ux ly))) ; -?

			((and (< lx 0) (> ux 0) (< ly 0) (> uy 0)) 
				(make-interval (min (* lx uy) (* ly ux))
							   (max (* lx ly) (* ux uy)))
			) ; ??
		)
	)
)

(display "ben")
(newline)

(mul-interval-ben (make-interval 1 2) (make-interval 3 4)) ; ++
(mul-interval-ben (make-interval 1 2) (make-interval -3 -4)) ; +-
(mul-interval-ben (make-interval -1 -2) (make-interval 3 4)) ; -+
(mul-interval-ben (make-interval -1 -2) (make-interval -3 -4)) ; -- 

(mul-interval-ben (make-interval -1 2) (make-interval 3 4)) ; ?+
(mul-interval-ben (make-interval 1 2) (make-interval -3 4)) ; +?

(mul-interval-ben (make-interval -1 2) (make-interval -3 -4)) ; ?-
(mul-interval-ben (make-interval -1 -2) (make-interval -3 4)) ; -?

(mul-interval-ben (make-interval -1 2) (make-interval -3 4)) ; ??
(mul-interval-ben (make-interval -5 5) (make-interval 3 4)) ; ??

(display "check")
(newline)

(mul-interval (make-interval 1 2) (make-interval 3 4)) ; ++
(mul-interval (make-interval 1 2) (make-interval -3 -4)) ; +-
(mul-interval (make-interval -1 -2) (make-interval 3 4)) ; -+
(mul-interval (make-interval -1 -2) (make-interval -3 -4)) ; -- 

(mul-interval (make-interval -1 2) (make-interval 3 4)) ; ?+
(mul-interval (make-interval 1 2) (make-interval -3 4)) ; +?

(mul-interval (make-interval -1 2) (make-interval -3 -4)) ; ?-
(mul-interval (make-interval -1 -2) (make-interval -3 4)) ; -?

(mul-interval (make-interval -1 2) (make-interval -3 4)) ; ??
(mul-interval (make-interval -5 5) (make-interval 3 4)) ; ??

(display "end")
(newline)