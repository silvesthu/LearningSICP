#lang racket
(require racket/stream)

(define (merge s1 s2)
	(cond 
		((stream-empty? s1) s2)
		((stream-empty? s2) s1)
		(else 
			(let 
				((s1car (stream-first s1))
				 (s2car (stream-first s2)))
				(cond 
					((< s1car s2car)
						(stream-cons s1car (merge (stream-rest s1) s2)))
					((> s1car s2car)
						(stream-cons s2car (merge s1 (stream-rest s2))))
					(else
						(stream-cons s1car
							(merge 
								(stream-rest s1)
								(stream-rest s2)))
					)
				)
			)
		)
	)
)

(stream->list (merge (in-range 1 5) (in-range 1 5)))

(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream))

(define S (stream-cons 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

; a combination of 2^x * 3^y * 5^z => step x / y / z

(stream-ref S 0)
(stream-ref S 1)
(stream-ref S 2)
(stream-ref S 3)
(stream-ref S 4)
(stream-ref S 5)
(stream-ref S 6)
(stream-ref S 7)
(stream-ref S 8)
(stream-ref S 9)
(stream-ref S 10)
