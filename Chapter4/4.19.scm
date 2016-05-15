#lang scheme

(include "header.scm")

(eval@ 
	'(let ((a 1))
		(define (f x)
			(define b (+ a x))
			(define a 5)
			(+ a b))
		(f 10))
	the-global-environment)

; before 4.16 => output 16
; after 4.16 => output Unbound a

; I support the version gives 16... maybe since I come as a C++ programmer

; As in the footnote, 16 is considered as a wrong answer
; And 20 is correct but difficult to perfectly implement, so it should produce a error instead.

; To get result of 20, maybe with some deffered execution support
; such as when encountered an undefined var (a here), leave it as (delay a)
; then after first pass of evaluation finished, deal with them.
; But dependencies could be really difficult to solve if there are multiple vars depending on each other.