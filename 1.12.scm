#lang scheme

; pascal triangle

; recursive

(define (p row column)
  ; (display row) (display ",") (display column) (newline) ; display flow
  (cond ((= column 0) 1)
        ((= row column) 1)
        (else (+ (p (- row 1) column)
                 (p (- row 1) (- column 1))))))

(p 3 2)
(newline)

; hell of repeat calculation only for print result

(define (loopColumn index row)
  (display (p row index))
  ;(unless (= index row) (loopColumn (+ index 1) row)))
  (when (not (= index row)) 
  		(display ",")
  	    (loopColumn (+ index 1) row)))

(loopColumn 0 3)
(newline)
(loopColumn 0 4)
(newline)
(loopColumn 0 5)
(newline)
(loopColumn 0 6)
(newline)

; loop on row should be similar

; Q: but how to jump out of execute flow without a return value ?
; A: when and unless

