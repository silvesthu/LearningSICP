#lang racket
(require racket/stream)

; count of addition
; n(0) = 0
; n(1) = 0
; n(2) = 1
; n(x) = n(x-1) + n(x-2) + 1

; in similar shape as fibonacci
; as x grows n(x) grows -> ignore "+ 1"
; -> same as fibonacci -> grows exponentially and ratio go to golden ratio