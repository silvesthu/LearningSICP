#lang scheme

(include "point-segment.scm")

(print-point (make-point 1 10))
(print-point (make-point 2 (- 5)))

(print-point (midpoint-segment (make-segment (make-point 1 10) (make-point 2 (- 5)))))