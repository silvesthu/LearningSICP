;2.13.scm
#lang scheme

(include "interval.scm")

(define (par1 r1 r2)
	(div-interval 	(mul-interval r1 r2)
					(add-interval r1 r2))
)

(define (par2 r1 r2)
	(let ((one (make-interval 1 1)))
		(div-interval one
			(add-interval 	(div-interval one r1)
							(div-interval one r2)
			)
		)
	)
)

(par1 (make-interval 1.0 2.0) (make-interval 1.0 2.0))
(par2 (make-interval 1.0 2.0) (make-interval 1.0 2.0))

; in par1 0.25 means both interval are 1 in the upper part and both are 2 in the lower part
; which is not possible...

; par2 use each interval only once, so it should be correct

(let ((A (make-interval 1 2)))
(div-interval A A)) 
; this means A(1, 2) / A'(1, 2)
; but from the code it should mean A/A = 1

; which give the reason for 2.15

; as for 2.16..

; yes? save all interval, try every combinations... then compare all of them to get lower and upper