#lang scheme

(define counter-analyze 0)
(define counter-execute 0)

(include "header.scm")
(include "analyze.scm")

(time (eval-analyze
	'(
		begin 
			(define (fib n)
			  (if (<= n 2)
			      1
			      (+ (fib (- n 1)) (fib (- n 2)))))
			(fib 10)
			(fib 20)
			(fib 30)
			(newline)

	)
	the-global-environment))

; cpu time: 1093 real time: 1099 gc time: 16
(display counter-analyze)(newline) ; 2 -> call to analyze-sequence
(display counter-execute)(newline) ; 5872006

(define counter-eval 0)
(set! counter-execute 0)

(time (eval@
	'(
		begin 
			(define (fib n)
			  (if (<= n 2)
			      1
			      (+ (fib (- n 1)) (fib (- n 2)))))
			(fib 10)
			(fib 20)
			(fib 30)
			(newline)

	)
	the-global-environment))

; cpu time: 1297 real time: 1301 gc time: 16
(display counter-eval)(newline) ; 1677722 -> call to eval-sequence
(display counter-execute)(newline) ; 5872006

; difference on time is not obvious though, should see clear result on complex program