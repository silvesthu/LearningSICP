#lang scheme

(define (count-pairs x)
	(define (count-pairs-inner x traversed)
		(cond ((not (pair? x)) traversed)
			  ((memq x traversed) traversed)
			  (else 
			  	(let 
			  		((left (count-pairs-inner (car x) (append traversed (list x)))))
			  		(count-pairs-inner (cdr x) left)
			  	)
			  )
		)
	)
	(length (count-pairs-inner x '()))
)

; three
(count-pairs (cons (cons 1 2) (cons 3 4)))

; four
(define p (cons 1 2))
(count-pairs (cons p (cons 3 p)))

; seven
(define q (cons p p))
(count-pairs (cons q q))

;(length '())