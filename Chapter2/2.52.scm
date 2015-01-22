#lang scheme

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; wave data from http://lispor.is-programmer.com/posts/25406.html
(define wave-orgin-data '(
                          ((36 60) (40 50) (36 40) (42 40) (60 18))
                          ((60 10) (36 30) (44 0))
                          ((36 0)  (30 20) (24 0))
                          ((16 0)  (24 30) (20 36) (10 30) (0 42))
                          ((0 50)  (10 38) (20 40) (24 40) (20 50) (24 60))
                          ((28 42) (32 42))
                          )
)
(define wave-vects-data (map (lambda (vs)
                               (map (lambda (xy)
                                      (let ((x (car xy))
                                            (y (cadr xy)))
                                        (make-vect (/ x 60.0) (/ y 60.0))))
                                    vs))
                             wave-orgin-data))
 
(define wave-segments (foldr append
                                  '()
                                  (map (lambda (vs)
                                         (let loop ((xys vs))
                                           (if (null? (cdr xys))
                                               '()
                                               (cons (make-segment (car xys) (cadr xys))
                                                     (loop (cdr xys))))))
                                       wave-vects-data)))

(paint (segments->painter wave-segments))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let (( up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let (
              (corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))

(paint (corner-split einstein 3))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n)))
)

(paint (square-limit (rotate180 einstein) 2))