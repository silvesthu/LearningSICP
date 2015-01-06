#lang scheme

(include "../common/profile.scm")

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

; ----- put last quee at last

;(define (safe? k positions) 
;	(let 
;		((last-queen (last positions))) ; no better way here ?		
;		(if (and (pair? positions) (null? (cdr positions)))
;			#t
;			(and (check? (car positions) last-queen) (safe? k (cdr positions)))
;		)
;	)
;)
;(define (adjoin-position new-row k rest-of-queens) (append rest-of-queens (list (list new-row k)))) 

; ----- put last queen at first

(define! (safe? k positions)
	(let ((last-queen (car positions)))
		(define (safe-inner? queens)
			(if (null? queens)
				#t
				(and (check? (car queens) last-queen) (safe-inner? (cdr queens)))
			)
		)
		;(display "<") (display last-queen) (display ",") (display (cdr positions)) (display (safe-inner? (cdr positions))) (display ">")
		(safe-inner? (cdr positions))
	)
	;#t
)
(define! (adjoin-position new-row k rest-of-queens) (append (list (list new-row k)) rest-of-queens))

; position format as (x1 x2)
; one solution format as ((x1 x2) (y1 y2))
; all solutions format as (((x1 x2) (y1 y2)) ((x1' x2') (y1' y2')))

;---------------------------------------

; v2.42
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

; v2.43
(define (queens-3 board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list empty-board)
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
					(lambda (new-row)
						(map (lambda (rest-of-queens)
								(adjoin-position new-row k rest-of-queens))
						(queen-cols (- k 1))))	 
					(enumerate-interval 1 board-size)))))
	(queen-cols board-size))

(length (queens 2))

(profile 'allres)

(length (queens 3))

(profile 'allres)

(length (queens 4))

(profile 'allres)

(length (queens 5))

(profile 'allres)

(length (queens 6))

(profile 'allres)

(length (queens 7))

(profile 'allres)

(length (queens 8))

(profile 'allres)

(length (queens 9))

(profile 'allres)

(length (queens-3 2))

(profile 'allres)

(length (queens-3 3))

(profile 'allres)

(length (queens-3 4))

(profile 'allres)

(length (queens-3 5))

(profile 'allres)

(length (queens-3 6))

(profile 'allres)

(length (queens-3 7))

(profile 'allres)

;(length (queens-3 8))

;(profile 'allres)

(newline)

; -- 

; so far results seems ok

; for queens 3, 4, 5, 6

; v2.42
; adjoin-position : 18, 60, 220, 894
; v2.43
; adjoin-position : 60, 624, 8160, 128904

; From test result: about (0.16 * T^2)

; Consider flatmap as double loop

; v2.42
; foreach (rest)
;	foreach (boardsize)

; v2.43
; foreach (boardsize)
;	foreach (rest)

; (count rest) * (count boardsize) and (count boardsize) * (count rest) should be equal

; However, there is an recursive call to "queen-cols" inside.
; Basically, for v2.43, for each new-row, rest-of-queens will be recalculated. Which are identical for the new line...

; For any row, the calculation goes boardsize times.
; And it expand as a recursive tree which will result to O(T^T)

; O(T^T) can be calculated if safe? always return true
; v2.42 -> n + n^2 + n^3 + ... -> O(n) : https://oeis.org/A031972
; v2.43 -> n^(n+1) -> O(n^(n+1)) : https://oeis.org/A007778

; Even 8-queen is difficult to estimate since solution count on each row varies

;0
;#hash((safe? . 6) (adjoin-position . 6))
;0
;#hash((safe? . 18) (adjoin-position . 18))
;2
;#hash((safe? . 60) (adjoin-position . 60))
;10
;#hash((safe? . 220) (adjoin-position . 220))
;4
;#hash((safe? . 894) (adjoin-position . 894))
;40
;#hash((safe? . 3584) (adjoin-position . 3584))
;92
;#hash((safe? . 15720) (adjoin-position . 15720))
;0
;#hash((safe? . 8) (adjoin-position . 8))
;0
;#hash((safe? . 60) (adjoin-position . 60))
;2
;#hash((safe? . 624) (adjoin-position . 624))
;10
;#hash((safe? . 8160) (adjoin-position . 8160))
;4
;#hash((safe? . 128904) (adjoin-position . 128904))
;40
;#hash((safe? . 2390486) (adjoin-position . 2390486))
;92
;#hash((safe? . 50889536) (adjoin-position . 50889536))

; Test result give 50889536 / 15720 = 3237 or 0.2 * T^2 is still a good estimation 
; (maybe since n-queen solution is much faster than safe?-return-true condition, a bit more than O(n) compare to O(n^n) )
; (then 2.43 gives a bit more than O(n^2) compare to O(n^(n+1)))


