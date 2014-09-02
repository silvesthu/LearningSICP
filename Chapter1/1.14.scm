#lang scheme

; ----------- Implementation

(define (count-change amount)
  (cc amount 5))

(define x 0)

(define (cc amount kinds-of-coins)
  (set! x (+ x 1))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; ----------- Question 1

; Right as amount decrease, down as kinds-of-coins decrese

; 11,5 = 0
; 11,4 = 0
; 11,3 - 1,3 - 0,3 = 0
;        1,2 - 0,2 = 0
;        1,1 = 1
; 11,2 - 6,2 - 1,2 - 0,2 = 0
;              1,1 = 1
;        6,1 = ... = 1
; 11,1 = ... = 1
; 11,0 = 0

; Total = 4

; ----------- Question 2 - Analysis

; Assume x = amount is a large number >> kinds-of-coins

; The deepest tree is that decrese x by 1 each time until 0, and depth is x + 1, take it roughly as x

; The second deepest tree, will be the x - 5, then decrease by 1 each time until 0, the depth is x + 1 - 5, take it roughly as x - 5

; So the total amount will be x + (x-5) + (x-5*2) + ... + 0 -> (0 + x) * (x / 5 + 1) / 2 -> O(x^2)

; Coins of 10 or more may also provide a lot of possibilities

; Stucked a little bit here. Cheated wih Ref : http://www.billthelizard.com/2009/12/sicp-exercise-114-counting-change.html

; Take 1,5,10 (kinds-of-coins = 5) ; *** KEY POINT IS TO FOCUS ON THAT X GOES INFINITY, AND THIS IS A RECURSION STRUCTURE

; For (x-10), it will generate an O(x^2) solutions. Also for (x-10*2), (x-10*3) ....

; There's (x / 10) of these repeation. So it becomes O(x^3)

; Then futher it will become O(x^4) for 4 kinds of coins.

; So the order of growth of number of steps is O(amount^5) -> generally O(amount^kinds-of-coins)

; As for the space, it will directly depend on the depth of tree, so it's O(amount)

; ----------- Record running times

(define (Run amount)
  (set! x 0) 
  (display (count-change amount)) 
  (display ", Steps = ") 
  (display x) 
  (display ", Amount = ") 
  (display amount) 
  (display ", Ratio = ") 
  (display (/ x (expt amount 5.0)))
  (newline))

(Run 50)
(Run 100)
(Run 200)
(Run 400)
(Run 800)