#lang scheme

(define COUNT 0)
(define COUNT_SINE 0)

(define (cube x) (* x x x))

(define (p x) 
	(set! COUNT (+ COUNT 1))
	(- (* 3 x) (* 4 (cube x))))

(define (sine angle)
	(if (not (> (abs angle) 0.01))
		angle
		(p (sine (/ angle 3.0)))))

; (sine (/ 3.1415926 (/ 180 30))) ; TEST : sin 30 degree -> 0.5

(set! COUNT 0)
(display "Sin(12.15) = ")(sine 12.15)
(display "Count = ")(display COUNT) ; Question a : number of p applied
(newline)
(newline)

; --------------

; guess ? O(logn)

(display "order of growth") (newline)

(set! COUNT 0)(display "Result = ")(sine 10    )(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(sine 100   )(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(sine 1000  )(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(sine 10000 )(display "Count = ")(display COUNT)(newline)
(set! COUNT 0)(display "Result = ")(sine 100000)(display "Count = ")(display COUNT)(newline)

; steps O(logn) <= a / (3^n) = 0.01

; step O(logn) <= every time calling sine, call stack + 1







