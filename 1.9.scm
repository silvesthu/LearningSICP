#lang scheme

; process 1

(define (dec a) (- a 1)) 
(define (inc a) (- a -1)) ; use - since + will be redefined

(define (+ a b)
  (if (= a 0)
  	  b
  	  (inc (+ (dec a) b)))) ; recursive (every 'inc' must be kept throught the calculation)

; process 2

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b)))) ; tail recursion -> iteration