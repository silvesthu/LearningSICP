#lang scheme

; for n = 5

;	A1,A2,A3,A4,A5(31)
;A5(16)		A1,A2,A3,A4(15)
;		A4(8)			A1,A2,A3(7)
;					A3(4)	 	A1,A2(3)
;							A1(1) 	  A2(2)

; for n = 10

; A10	 A(1~9)
;		 A9			A(1~8)
;					A8		A(1~7)
;		 					A7		A(1~6)
;									A6		A(1~5)
;											A5		A(1~4)
;													A4		A(1~3)
;															A3		A(1~2)
;																	A2		A1

; most frequent symbol : 1
; least frequent symbol : 9 (n - 1)

; since it always form the most unbalanced tree... (with leaf layer has two leaves)