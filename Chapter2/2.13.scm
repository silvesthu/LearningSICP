;2.13.scm
#lang scheme

(include "interval.scm")

(define (make-center-width c w)
	(make-interval (- c w) (+ c w))
)

(define (center i)
	(/ (+ (lower-bound i) (upper-bound i)) 2)
)

(define (width i)
	(/ (- (lower-bound i) (upper-bound i)) 2)
)

(define (make-center-percent c p)
	(let ((w (abs (* c p))))
		(make-center-width c w)
	)
)

; assume all numbers are positive

; give center, percent pair (x, p) (y, q)

; result interval is (x - xp) * (y - yq) ~ (x + xp) * (y + yq)

; result percent is 2 * (p + q) / (pq - 1)