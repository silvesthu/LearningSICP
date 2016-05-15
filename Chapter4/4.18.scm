#lang scheme

(lambda (f y0 dt)
	(let 
		(
			(y '*unassigned*)
			(dy '*unassigned*))
	(let (
			(a (integral (delay dy) y0 dt))
			(b (stream-map f y)) ; y in this line gives '*unassigned*
	(set! y a)
	(set! dy b)
	y))))

; will not work