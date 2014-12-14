#lang scheme

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))
	)
)

(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest))
				  (cdr rest)
			)
		)
	)
	(iter initial sequence)
)

(define fold-right accumulate)

; --------------------------------------

(define (reverse sequence)
	(fold-right 
		(lambda (x y) 
			(display "x=")(display x)(display " y=")(display y) (newline)
			(append y (list x))
			; took a while to recall there is a "append"...
			; although it just work as a "push_back" 
		)
		null 
		sequence
	)
)

(reverse '(1 2 3))

(define (reverse-l sequence)
	(fold-left (lambda (x y) (cons y x)) null sequence)
)

(reverse-l '(1 2 3))