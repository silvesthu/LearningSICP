#lang scheme

(define y 100)

; hold a reference to outer varaible...
(define (f data)  
  (set! y 
    (* data y)
  )
  y
)

(+ (f 1) (f 0))
(+ (f 0) (f 1))