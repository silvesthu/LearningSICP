#lang scheme

; as pseudo code only

(define (make-queue)
	(let ((front-ptr '())
		  (rear-ptr '()))
		(define (empty?) (null? front-ptr))
		(define (front) (if (empty?) (error) (car front-ptr)))
		(define (insert! v) 
			(let ((new-pair (cons item '())))
				(cond ((empty)
						(set! front-ptr new-pair)
					    (set! rear-ptr new-pair))
					  (else
					  	(set-cdr! rear-ptr new-pair)
					  	(set! rear-ptr new-pair)
					  ))))
		(define (delete! v)
			(cond 
				((empty) (error "empty"))
				(else (set! front-ptr (cdr front-ptr)))
			)
		)
		(define (dispatch m) 
			(cond 
				((eq? m 'empty?) empty?)
				((eq? m 'front) front)
				((eq? m 'insert!) insert!)
				((eq? m 'delete!) delete!)
			)
		)
		dispatch
	)
)