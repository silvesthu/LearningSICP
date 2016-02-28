#lang racket
(require racket/stream)

(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
	(average guess (/ x guess)))

;(define (sqrt-stream x)
;	(define guesses
;		(stream-cons 1.0
;			(stream-map 
;				(lambda (guess) 
;					(display "guess = ") (display guess) (display ", x = ") (display x) (newline) 
;					(sqrt-improve guess x))
;				guesses)))
;	guesses)

(define (sqrt-stream x)
	(stream-cons 1.0
		(stream-map 
			(lambda (guess) (display "guess = ") (display guess) (display ", x = ") (display x) (newline) 
			(sqrt-improve guess x))
			(sqrt-stream x))))

(define sqrt2 (sqrt-stream 2))
(stream-ref sqrt2 0)
(stream-ref sqrt2 1)
(stream-ref sqrt2 2)
(stream-ref sqrt2 3)

; explain
; if not define guesses locally, the function return a stream with a delay-evaluation inside.
; then when it is called second time, it will be evaluated again even the result is cached.

; Hint for self: in C# store a IEnumerable<T> or using .ToList() to turn it to a List<T>

; no, they will be the same