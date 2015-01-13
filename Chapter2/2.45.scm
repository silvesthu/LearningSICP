#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
;(paint (number->painter 0))
;(paint diagonal-shading)
;(paint-hires  (below (beside diagonal-shading
;                             (rotate90 diagonal-shading))
;                     (beside (rotate270 diagonal-shading)
;                             (rotate180 diagonal-shading))))
;(paint einstein)

(define (split op1 op2)
  (lambda (painter n) 
    (if (= n 0)
      painter
      (let ((smaller ((split op1 op2) painter (- n 1))))
        (op1 painter (op2 smaller smaller))))
  )  
)

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 1))
(paint (up-split einstein 1))
(paint (up-split einstein 2))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let (( up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(display "corner")
(paint (corner-split einstein 1))