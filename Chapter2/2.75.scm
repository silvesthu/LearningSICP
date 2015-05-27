#lang scheme

(define (make-from-mag-ang mag ang)
	(define (dispatch op)
		(cond 
			((eq? op 'real-part) (* mag (cos ang)))
			((eq? op 'imag-part) (* mag (sin ang)))
			((eq? op 'magnitude) mag)
			((eq? op 'angle) ang)
			(else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))
		)
	)
	dispatch ; return the dispatch function
)

(define (apply-generic op arg) (arg op))

(apply-generic 'magnitude (make-from-mag-ang 2 3.1415926))
(apply-generic 'angle	  (make-from-mag-ang 2 3.1415926))
(apply-generic 'real-part (make-from-mag-ang 2 3.1415926))
(apply-generic 'imag-part (make-from-mag-ang 2 3.1415926))