#lang scheme

(define (good-enough? guess last-guess) ; compare to last guess
  (< (abs (- guess last-guess)) 0.0001)) ; epsilon

(define (cube-iter guess last-guess x)
  (if (good-enough? guess last-guess)
          guess
          (cube-iter (improve guess x) guess x)))

(define (cube x) (cube-iter 1.0 0 x))

; -------- based on 1.7 above

(define (improve guess x) ; get a better approx
  (/ (+ (/ x (* guess guess))
  	    (* guess 2))
  	 3)
)

(cube 2)

