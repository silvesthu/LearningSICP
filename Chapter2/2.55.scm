#lang scheme

(car ''abracadabra)

; => (car (quote (quote abracadabra)))
; => quote

(car '(print 1)) ; print
(cdr '(print 1)) ; (1) in a list <- quote also onto 1
