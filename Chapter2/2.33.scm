#lang scheme

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))
	)
)

(define (map p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) null sequence)
)

(map (lambda (x) (* x x)) (list 1 2 3))

; ----------------------------

(define (append seq1 seq2)
	(accumulate cons seq2 seq1)
)

; structure of accumulate makes sure last element in seq2 
; comes first to connect with seq1

(append (list 1 2 3) (list 4 5 6))

; ----------------------------

(define (length sequence)
	(accumulate (lambda (x y) (+ 1 y)) 0 sequence)
)

(length (list 1 2 3 4 5 6 7))