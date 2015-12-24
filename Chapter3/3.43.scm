#lang scheme

; fetching the balance and proceed withdraw or deposit is seperated
; so incorrect balance may used to calculate difference

; e.g.
; 1. diff = 20 - 10 = 10
; 2. diff = 30 - 10 = 20
; 3. exchange : 20 -> 10 , 10 -> 20
; 4. exchange : 30 -> 10 , 20 -> 40
; 5. result: 10 10 40

; however, withdraw and deposit do not modify balance directly, they only perform as +x / -x,
; so as long as they are in pair, the total amount will be preserved

; if trasactions are not serialized on individual accounts
; it is still possible to fetch a balance and set it to two different values which will change the amount