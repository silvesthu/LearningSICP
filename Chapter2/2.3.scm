#lang scheme

(include "basic.scm")
(include "point-segment.scm")

(define (make-rectange-cornor cornor vec1 vec2)
	(let 
		((half-vec1 (mul-point vec1 (/ 1 2)))
		(half-vec2 (mul-point vec2 (/ 1 2))))

		(list 
			(add-point (add-point cornor half-vec1) half-vec2)
			half-vec1 
			half-vec2
		)
	)
)

(define (make-rectange-center center half-vec1 half-vec2)
	(list center half-vec1 half-vec2)
)

(define (extendA-rectange rectange)
	(make-segment (car rectange) (add-point (car rectange) (cadr rectange)))
)

(define (extendB-rectange rectange)
	(make-segment (car rectange) (add-point (car rectange) (caddr rectange)))  ; cadr -> (car (cdr))
)

(define (lengthA-rectange rectange)
	(length-segment (extendA-rectange rectange))
)

(define (lengthB-rectange rectange)
	(length-segment (extendB-rectange rectange))
)

(define (perimeter-rectange rectange)
	(* (+ (lengthA-rectange rectange) (lengthB-rectange rectange)) 2)
)

(define (area-rectange rectange)
	(* (lengthA-rectange rectange) (lengthB-rectange rectange))
)

(define (create-rect1)
	(make-rectange-center (make-point 1 1) (make-point 1 1) (make-point 2 (- 2)))
)

(define (create-rect2)
	(make-rectange-cornor (make-point 4 0) (make-point (- 2) (- 2)) (make-point (- 4) (+ 4)))
)

(create-rect1)
(extendA-rectange (create-rect1))
(extendB-rectange (create-rect1))
(length-segment (extendA-rectange (create-rect1)))
(length-segment (extendB-rectange (create-rect1)))
(perimeter-rectange (create-rect1))
(area-rectange (create-rect1))

(create-rect2)
(extendA-rectange (create-rect2))
(extendB-rectange (create-rect2))
(perimeter-rectange (create-rect2))
(area-rectange (create-rect2))