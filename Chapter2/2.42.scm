#lang scheme

(define (enumerate-interval l r)
	(define (enumerate-interval-inner l r result)
		(if (< r l)
			result
			(enumerate-interval-inner l (- r 1) (cons r result))
		)
	)
	(enumerate-interval-inner l r null)
)

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))
	)
)

(define (flatmap proc seq)
	(accumulate append null (map proc seq))
)

;---------------------------------------

(define (check? pos1 pos2)
	(if 
		(or 
			(= (car pos1) (car pos2))
			(= (cadr pos1) (cadr pos2))
			(= (- (car pos1) (car pos2)) (- (cadr pos1) (cadr pos2)))
			(= (- (car pos1) (car pos2)) (- (cadr pos2) (cadr pos1)))
		)
		#f
		#t
	)
)

(define (last l)
	(if (null? (cdr l))
		(car l)
		(last (cdr l))
	)
)

(define empty-board null)
(define (safe? k positions) 
	(let 
		((last-queen (last positions))) ; no better way here ?		
		(if (and (pair? positions) (null? (cdr positions)))
			#t
			(and (check? (car positions) last-queen) (safe? k (cdr positions)))
		)
	)
)

(define (adjoin-position new-row k rest-of-queens) (append rest-of-queens (list (list new-row k)))) ; 

; position format as (x1 x2)
; one solution format as ((x1 x2) (y1 y2))
; all solutions format as (((x1 x2) (y1 y2)) ((x1' x2') (y1' y2')))

;---------------------------------------

(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list empty-board)
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
					(lambda (rest-of-queens)
						(map (lambda (new-row)
								(adjoin-position new-row k rest-of-queens))
							 (enumerate-interval 1 board-size)))
					(queen-cols (- k 1))))))
	(queen-cols board-size))

(queens 0)
(queens 1)
(queens 2)
(queens 3)
(queens 4)
(length (queens 7))
(length (queens 8))
(length (queens 10))