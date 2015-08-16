#lang scheme

(define (make-monitored f)
	(define count 0)
	(lambda (x . y)
		(if (eq? x 'how-many-calls?)
			count
			(begin (set! count (+ count 1)) (apply f (append (list x) y)))
		)		
	)
)

(define s (make-monitored sqrt))

(s 100)

(s 9)

(s 'how-many-calls?)