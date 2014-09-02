#lang scheme

(define (make-rat n d)
	(let ((g (gcd n d)))
		(cond 
			((= d 0) (error "div 0"))
			((< d 0) (cons (/ (- 0 n) g) (/ (- 0 d) g)))
			((> d 0) (cons (/ n g) (/ d g)))
		)
	)
)


(make-rat 1 2)
(make-rat -1 -2)
(make-rat 1 -2)
(make-rat -1 2)

(make-rat 1 0)
(newline)