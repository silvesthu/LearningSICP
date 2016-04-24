#lang scheme

(include "header.scm")

(eval@ '(define x 0) the-global-environment)
(eval@ '(+ 1 2) the-global-environment)
(eval@ '(list 1 2) the-global-environment)

; chose for cuz it has the most complete form...
; otherwise need to deal with frame...

(define (for->combination exp)
	;(display exp) (newline)
	(let 
		(
			(assignments (cadr exp))
			(conditions (caddr exp))
			(statements (cadddr exp))
		)
		(define core
			(append 
				; init assignments
				(if 
					(or (empty? assignments) (empty? (car assignments)))
					'()
					(map (lambda (a) (append (list 'define) a)) assignments)
				)
				(list 
					; loop
					(list 'if
						(append (list 'or) conditions)
						; execute and call "for" recursively without init
						(sequence->exp 
							(append 
								(map (lambda (s) (append (list 'set!) s)) statements)
								(list (list 'for (append (list (list))) conditions statements))
							)
						)
						; return value of last statement
						(car (car (reverse statements)))
					)
				)
			)
		)
		;(display "<for->combination>") (newline)
		;(display core) (newline)
		;(display "</for->combination>") (newline)
		(sequence->exp core)
	)	
)

(put-keyword 'for (lambda (exp env) (eval@ (for->combination exp) env)))
(eval@ '(for
  ((x 0) (y 0)) ; assignment
  ((< x 10)) ; or condition
  ((x (+ x 1)) (y (+ y 10))) ; statement
)
the-global-environment)