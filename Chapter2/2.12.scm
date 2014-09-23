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

(make-center-percent 100 0.1) ; but what if center is 0 ?