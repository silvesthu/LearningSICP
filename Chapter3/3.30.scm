#lang scheme 

; half adder : C = or / S = and + or * 2 + not
; full adder : C = and + or * 3 + not / S = and + or * 2 + not
; RCAk = full adder * k =>
; 	S = time of finishing Ck-1 + S from a full adder = and * (k-1) + or * 3 * (k-1) + not * (k-1) + and + or * 2 + not
;   C = time of finishing Ck-1 + C from a full adder = and * (k-1) + or * 3 * (k-1) + not * (k-1) + and + or * 3 + not

(define (ripple-carry-adder Ak Bk Sk C)
	(cond 
		((pair? Ak)
			(let ((c-out (make-wire)))
				(full-adder (car Ak) (car Bk) C (car Sk) c-out)
				(ripple-carry-adder (cdr Ak) (cdr Bk) (cdr Sk) c-out)
			))
		((null? Ak) ; -> this can be simplified cuz input should be list which means not null in first iteration
			'Done)
		(else
			(let ((c-out (make-wire)))
				(full-adder Ak Bk C Sk c-out))
		)
	)
)