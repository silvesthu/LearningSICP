#lang scheme

(define (square x) (* x x))

(define (even? n)
	(= (remainder n 2) 0))

(define (fast-expt b n)
	(cond ((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

(fast-expt 2 10)
(fast-expt 2 40)

; ---------

(define COUNT 0)

(define (iter-expt a b n)
	(set! COUNT (+ COUNT 1))
	(cond ((= n 0) a)
		((even? n) (iter-expt a (square b) (/ n 2))) ; tail call
		(else (iter-expt (* a b) b (- n 1))))) ; tail call

; (iter-expt 1 2 1)
; (iter-expt 1 2 10)
(newline)

(display "order of growth") (newline)

(set! COUNT 0)(display "Result = ")(iter-expt 1 2 20)(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(iter-expt 1 2 40)(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(iter-expt 1 2 80)(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(iter-expt 1 2 160)(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(iter-expt 1 2 320)(display "Count = ")(display COUNT)(newline)