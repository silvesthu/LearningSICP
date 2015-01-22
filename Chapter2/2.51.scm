#lang scheme

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (transform-painter~ painter origin corner1 corner2)
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

(define (below1 painter1 painter2)
	(let ((split-point (make-vect 0.0 0.5)))
          (let ((paint-left
                 (transform-painter~ painter1
                                    (make-vect 0.0 0.0)
                                    (make-vect 1.0 0.0)
                                    split-point))
                (paint-right
                 (transform-painter~ painter2
                                    split-point
                                    (make-vect 1.0 0.5)
                                    (make-vect 0.0 1.0))))
            (lambda (frame)
              (paint-left frame)
              (paint-right frame)))))

(define (below2 painter1 painter2)
  (lambda (frame)
    (
     (rotate90 (beside (rotate270 painter1) (rotate270 painter2)))
     frame
    )
  )
)

; (paint (beside einstein einstein))
(paint (below1 einstein einstein))
(paint (below2 einstein einstein))