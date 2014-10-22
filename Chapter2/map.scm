(define (filter pred x)
	(cond 
		((null? x) x)
		((pred (car x)) (cons (car x) (filter pred (cdr x))))
		(else (filter pred (cdr x)))
	)
)

(define (map proc x)
	(cond 
		((null? x) x)
		(else (cons (proc (car x)) (filter proc (cdr x))))
	)
)