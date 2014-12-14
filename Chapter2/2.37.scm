#lang scheme

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))
	)
)

(define (accumulate-n op initial sequence)
	(if (null? (car sequence))
		null
		(cons 	(accumulate op initial (map (lambda (s) (car s)) sequence))
				(accumulate-n op initial (map (lambda (s) (cdr s)) sequence))
		)
	)
)

; ============================

(define (dot-product v w)
	(accumulate + 0 (map * v w))
)

; ----------------------------

(define v0 '(1 1 1 1))
(define v1 '(1 2 3 4))
(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(dot-product v0 v1) ; ok

; ----------------------------

(define (matrix-*-vector m v)
	(map (lambda (x) (dot-product v x)) m)
)

(matrix-*-vector m v0) ; ok

; ----------------------------

(define (transpose mat)
	(accumulate-n cons null mat)
)

m
(transpose m) ; ok

(display "-----------")(newline)

; ----------------------------

(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
		(map (lambda (row) (matrix-*-vector cols row)) m)
	)
)

m
(matrix-*-matrix m (transpose m)) ; ok. (checked on {{1,2,3,4},{4,5,6,6},{6,7,8,9}}.transpose({{1,2,3,4},{4,5,6,6},{6,7,8,9}}))

;http://www.wolframalpha.com/input/?i=%7B%7B1%2C2%2C3%2C4%7D%2C%7B4%2C5%2C6%2C6%7D%2C%7B6%2C7%2C8%2C9%7D%7D.transpose%28%7B%7B1%2C2%2C3%2C4%7D%2C%7B4%2C5%2C6%2C6%7D%2C%7B6%2C7%2C8%2C9%7D%7D%29