#lang scheme

; the enclosing let is actually a procedure call which create a extra frame
; it should be possible to bring all define to the front, then evaluate other statements