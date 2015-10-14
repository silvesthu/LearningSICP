#lang scheme

; global (x, z, set-car!)

; E1 x -> function dispatch -> (x = 1, y = 2)
; E2 z -> function dispatch -> (x = x↑, y = x↑)

; E3 set-car! -> (z = (cdr z), new-value = 17) -> (z = x↑, new-value = 17) -> set-x! (x = 1, y = 2, v = 17)
	; E1 x -> function dispatch -> (x = 17, y = 2)

; E4 car -> x (x = 17, y = 2) => 17