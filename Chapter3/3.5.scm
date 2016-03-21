#lang scheme

;(require srfi/1)
(require racket/base)

(define (rand)
	(inexact->exact (round (* (random) 2147483647)))
)

(define (estimate-pi trials)
	(sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
	(= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
	(define (iter trials-remaining trials-passed)
		(cond ((= trials-remaining 0)
			(/ trials-passed trials))
		((experiment)
			(iter (- trials-remaining 1) (+ trials-passed 1)))
		(else
			(iter (- trials-remaining 1) trials-passed))))
	(iter trials 0))

(estimate-pi 10000)

; this is an algorithm based on probability that if 2 number are coprime
; ref: https://en.wikipedia.org/wiki/Coprime_integers#Probabilities

; ---------------------------

(define (random-in-range low high)
	(let ((range (- high low)))
		(+ low (* range (random))))
)

(define (estimate-integral predict x1 x2 y1 y2 trial-count)
	(monte-carlo trial-count (lambda () 
								(predict 
									(random-in-range x1 x2)
									(random-in-range y1 y2)
								)
							)))

(define (square x) (* x x))
(define int (estimate-integral 
	(lambda (x y) (<= (+ (square (- x 5)) (square (- y 7))) (square 3)))
	2 8 4 10 100000))

(* (exact->inexact int) 4) ; int = pi * (3 * 3) / (6 * 6)

