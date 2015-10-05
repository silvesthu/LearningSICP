#lang scheme

(define (infinity? x)
	(define (infinity?-inner x traversed)
		(cond ((not (pair? x)) traversed)
			  ((memq x traversed) #t)
			  (else 
			  	(let 
			  		((left (infinity?-inner (car x) (append traversed (list x)))))
			  		(cond 
			  			((eq? left #t) #t)
			  			(else
			  				(infinity?-inner (cdr x) left)
			  			)
			  		)
			  		
			  	)
			  )
		)
	)
	(let ((result (infinity?-inner x '())))
		(cond 
			((eq? result #f) #f)
			(else #t)
		)
	)
)

(infinity? '())
(infinity? '(1 2 3))

; too complex....