; arithmetic
(add 1 2)                 ; 3
(sub 5 2)                 ; 3
(mult 3 4)                ; 12
(div 10 2)                ; 5
(mod 10 3)                ; 1

; comparisons
(gt 5 2)                  ; true
(lt 3 7)                  ; true
(eq 4 4)                  ; true
(neq 1 2)                 ; true
(ge 5 5)                  ; true
(le 2 3)                  ; true

; boolean logic
(not true)                 ; false
(and true false)           ; false
(or false true)            ; true

; conditionals
(if (gt 3 2) 
    (int 1) 
    (int 0))               ; 1

; conversions
(int 3.7)                  ; 3
(float 4)                  ; 4.0

; variable bindings
(let x (add 2 3) 
    (mult x 2))             ; 10

; functions
(let square (fun (n) (mult n n))
    (square 5))             ; 25

; nested function and let
(let double (fun (x) (mult x 2))
    (let y (double 10)
        (add y 3)))         ; 23
