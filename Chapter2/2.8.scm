#lang scheme

(include "interval.scm")

; minimun value of the difference could be the the difference of lower-upper of x and upper-bound y
; maximum value of the difference could be the the difference of upper-upper of x and lower-bound y

(sub-interval (make-interval 1 2) (make-interval 0 4))
