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

