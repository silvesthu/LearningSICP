#lang scheme

(define (make-mobile left right)
	(list left right)
)

(define (make-branch length structure)
	(list length structure)
)

; ---

; a

(define (left-branch mobile)
	(car mobile)
)

(define (right-branch mobile)
	(cadr mobile)
)

; b

(define (3-4-5)
	(make-mobile (make-branch 1 (make-mobile (make-branch 13 3) (make-branch 14 4))) (make-branch 2 5))
)

(define (total-weight-inner structure isMobile)
	(display structure)(display " ")(display isMobile)(newline)
	(if isMobile
		(+ (total-weight-inner (left-branch structure) #f) (total-weight-inner (right-branch structure) #f))
		(if (pair? (cadr structure)) ; is mobile or weight
			(total-weight-inner (cadr structure) #t)
			(cadr structure)
		)
	)
)

(define (total-weight mobile)
	(total-weight-inner mobile #t)
)

(define (balanced? mobile)
	(let 
		(
			(left (* (total-weight-inner (left-branch mobile) #f) (car (left-branch mobile))))
			(right (* (total-weight-inner (right-branch mobile) #f) (car (right-branch mobile))))
		)
		(display left)
		(display right)
		(= left right)
	)
)

(define (6-4-5-b)
	(make-mobile (make-branch 1 (make-mobile (make-branch 2 6) (make-branch 1 4))) (make-branch 2 5))
)

; (3-4-5)
(total-weight (3-4-5))
(balanced? (6-4-5-b)) ; so length of sub-branch is ignored ?

;--------------------------------------------------------------------------------------

(define (make-mobile-c left right)
	(cons left right)
)

(define (make-branch-c length structure)
	(cons length structure)
)


; ---

; a

(define (left-branch-c mobile)
	(car mobile)
)

(define (right-branch-c mobile)
	(cdr mobile)
)

; b

(define (3-4-5-c)
	(make-mobile-c (make-branch-c 1 (make-mobile-c (make-branch-c 13 3) (make-branch-c 14 4))) (make-branch-c 2 5))
)

(define (total-weight-inner-c structure isMobile)
	(display structure)(display " ")(display isMobile)(newline)
	(if isMobile
		(+ (total-weight-inner-c (left-branch-c structure) #f) (total-weight-inner-c (right-branch-c structure) #f))
		(if (pair? (cdr structure)) ; is mobile or weight
			(total-weight-inner-c (cdr structure) #t)
			(cdr structure)
		)
	)
)

(define (total-weight-c mobile)
	(total-weight-inner-c mobile #t)
)

(define (balanced?-c mobile)
	(let 
		(
			(left (* (total-weight-inner-c (left-branch-c mobile) #f) (car (left-branch-c mobile))))
			(right (* (total-weight-inner-c (right-branch-c mobile) #f) (car (right-branch-c mobile))))
		)
		(display left)
		(display right)
		(= left right)
	)
)

(define (6-4-5-b-c)
	(make-mobile-c (make-branch-c 1 (make-mobile-c (make-branch-c 2 6) (make-branch-c 1 4))) (make-branch-c 2 5))
)

; (3-4-5)
(total-weight-c (3-4-5-c))
(balanced?-c (6-4-5-b-c)) ; so length of sub-branch is ignored ?

; turn all cadr -> cdr => it could be a (second) or (right)