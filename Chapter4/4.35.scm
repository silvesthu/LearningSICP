#lang racket

(include "header.scm")

(define retry-count 3)

(define on-success
	(lambda (val next-alternative)
		(if (> retry-count 0)
			(begin 
				(set! retry-count (- retry-count 1))
				(display "-- retry --") (newline)
				(next-alternative)
				(newline)
			)
			#f
		)
	)
)

(ambeval '
  (begin
  	(define (require p)
  		(if (not p) (amb))
  	)

    (define (an-integer-between a b)
		(require (<= a b))
		(amb a (an-integer-between (+ a 1) b))
	)

	(display (an-integer-between 1 5)) (newline)
  )
the-global-environment on-success (lambda () (newline)))


