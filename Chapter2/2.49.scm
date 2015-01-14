#lang scheme
;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))


(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v) (make-vect (* (xcor-vect v) s) (* (ycor-vect v) s)))


(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (frame-coord-map frame)
	(lambda (v)
		(add-vect
			(origin-frame frame)
			(add-vect 	(scale-vect (xcor-vect v)
									(edge1-frame frame))
						(scale-vect (ycor-vect v)
									(edge2-frame frame))))))

(define (draw-line v1 v2) (display "drawing ") (display v1) (display "\t->\t") (display v2) (newline))

(define (segments->painter segment-list)
	(lambda (frame)
		(for-each
			(lambda (segment)
				(draw-line
					((frame-coord-map frame) (start-segment segment))
					((frame-coord-map frame) (end-segment segment))))
			segment-list)))

; helper

(define (p0) (make-vect 0 0))
(define (p1) (make-vect 1 0))
(define (p2) (make-vect 0 1))
(define (p3) (make-vect 1　1))

(define (mid-vect v1 v2) (scale-vect (add-vect v1 v2) 0.5))

(define (draw-frame frame) 
	((segments->painter 
		(list 
			(make-segment (p0) (p1))
			(make-segment (p0) (p2))
			(make-segment (p3) (p1))
			(make-segment (p3) (p2))
		)
	) frame)
)
(define (draw-x frame)
	((segments->painter 
		(list 
			(make-segment (p0) (p3))
			(make-segment (p1) (p2))
		)
	) frame)
)

(define (mid01) (mid-vect (p0) (p1)))
(define (mid02) (mid-vect (p0) (p2)))
(define (mid31) (mid-vect (p3) (p1)))
(define (mid32) (mid-vect (p3) (p2)))

(define (draw-diamond frame)
	(segments->painter 
		(list 
			(make-segment (mid01) (mid02))
			(make-segment (mid01) (mid31))
			(make-segment (mid02) (mid32))
			(make-segment (mid31) (mid32))
		)
	)
)

(define (make-segment-xy x1 y1 x2 y2) 
	(make-segment 
		(make-vect x1 y1)
		(make-vect x2 y2)
	)
)

(define (draw-wave frame)
	((segments->painter 
		(list 
			; left arm
			(make-segment-xy 0 		0.6 	0.1 	0.4)
			(make-segment-xy 0.3 	0.56 	0.1 	0.4)
			(make-segment-xy 0.3 	0.56 	0.4 	0.5)
			(make-segment-xy 0	 	0.85 	0.1 	0.6)
			(make-segment-xy 0.3 	0.6 	0.1 	0.6)
			(make-segment-xy 0.3 	0.6 	0.45 	0.6)

			; blabla..
		)
	) frame)
)

(draw-wave (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))