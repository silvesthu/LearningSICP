#lang scheme

; recursive multiply

(define (double x) (+ x x))

(define (even? n)
	(= (remainder n 2) 0))

(define COUNT 0)

(define (* a b)
	(set! COUNT (+ COUNT 1))
	(cond ((= b 0) 0)
		((even? b) (double (* a (/ b 2))))
		(else (+ a (* a (- b 1))))))

(* 3 5) ; Random Test
(* 11 11) ; Random Test

; why provide halve ?

(set! COUNT 0)(display "Result = ")(* 1 20)(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(* 1 40)(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(* 1 80)(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(* 1 160)(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(* 1 320)(display "Count = ")(display COUNT)(newline)

; logarithmic number of steps
