#lang scheme

(include "header.scm")
(include "analyze.scm")

; (eval-analyze
; 	'(display (+ 1 2 3))
; 	the-global-environment)

(newline)

(eval@
	'(let 
		(
			(a 1)
		 	(b (* 3 3))
		)
		(+ a b)
	)
	the-global-environment)

(eval-analyze
	'(let 
		(
			(a 1)
		 	(b (* 3 3))
		)
		(+ a b)
	)
	the-global-environment)
