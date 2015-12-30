#lang scheme

; pseudo code

(define (make-semaphone max)
	(define counter-max max)
	(define counter 0)
	(define counter-lock (make-mutex))
	(define lock (make-mutex))
	(define (acquire-inner)
		(lock 'acquire)
		(counter-lock 'acquire)
	)
	(define (acquire)
		(counter-lock 'acquire)
		(if (>= counter counter-max)
			(counter-lock 'release)
			(acquire-inner)
		)
		(set! counter (+ counter 1))
		(counter-lock 'release)
	)
	(define (release)
		(lock 'release)
		(counter-lock 'acquire)
		(set! counter (- counter 1))
		(counter-lock 'release)
	)
	(define (the-semaphone s)
		(cond 
			((eq? m 'acquire) (acquire))
			((eq? m 'release) (release))
		)
	)
)

; on test-and-set!

; just embed code of mutex in the book ?