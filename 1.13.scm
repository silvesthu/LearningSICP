; Prove Fib(n) is the closest integer to phi^n / sqrt(5), where phi = (1 + sqrt(5)) / 5

; Let psi = (1 - sqrt(5)) / 5

; Hint: induction -> prove Fib(n) = (phi^n - psi^n) / sqrt(5)

; -----------------

; Step 1 - induction

; Let Fib -> (1, 1, 2, 3, 5, 8, ...), index starts at 1

; Fib(1) = 1 = (phi^1 - psi^1) / sqrt(5)
; Fib(2) = 1 = (phi^2 - psi^2) / sqrt(5)

; Suppose Fib(k) = (phi^k - psi^k) / sqrt(5)
; Suppose Fib(k+1) = (phi^(k+1) - psi^(k+1)) / sqrt(5)

; Check Fib(k+2) = (phi^(k+2) - psi^(k+2)) / sqrt(5)

; Fib(k+2) = Fib(k) + Fib(k+1)
;		   = (phi^k - psi^k) / sqrt(5) + (phi^(k+1) - psi^(k+1)) / sqrt(5)
;		   = (phi^k * (1 + phi) - psi^k * (1 + psi)) / sqrt(5) ; there is 1 + phi = phi^2
;		   = (phi^(k+2) - psi^(k+2)) / sqrt(5)				   ; and alos 1 + psi = psi^2

; Q.E.D

; ----------------

; Step 2

; Then to prove the original problem
; closest integer -> fabs(Fib(n) - phi^n / sqrt(5)) < 0.5

; Which is fabs((phi^n - psi^n) / sqrt(5) -  phi^n / sqrt(5)) < 0.5
; = fabs(psi^n / sqrt(5)) < 0.5 ; since psi < 1 so psi^n goes down as psi goes up, and when n = 1 it's true.
; = true

; Q.E.D