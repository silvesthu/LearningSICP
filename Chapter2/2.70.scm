#lang scheme

(require "2.68.scm")
(require "2.69.scm")

(define tree (generate-huffman-tree (list 
'(A 		2) 
'(NA 		16) 
'(BOOM 		1) 
'(SHA 		3)
'(GET 		2) 
'(YIP 		9) 
'(JOB 		2) 
'(WAH 		1)
)))

(define lyric
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

(encode lyric tree)
(length (encode lyric tree))

 ; 3 bit for 8 variations
(print (* (length lyric) 3)) (display " with fixed-length(3) code") (newline)