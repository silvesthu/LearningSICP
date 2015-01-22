#lang scheme

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v s) (make-vect (* (xcor-vect v) s) (* (ycor-vect v) s)))

(define (make-frame origin edge1 edge2)
	(list origin edge1 edge2)
)

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
									(edge2-frame))))))

(define (transform-painter painter origin corner1 corner2)
	(lambda (frame)
		(let ((m (frame-coord-map frame)))
			(let ((new-origin (m origin)))
				(painter
					(make-frame new-origin
						(sub-vect (m corner1) new-origin)
						(sub-vect (m corner2) new-origin)
					)
				)
			)
		)
	)
)

(define (flip-horiz painter)
	(transform-painter painter
			(make-vect 1.0 0.0)
			(make-vect 0.0 0.0)
			(make-vect 1.0 1.0)
	)
)