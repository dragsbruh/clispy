# clispy

lisp-like language for a beam-like runtime
neither the language nor the runtime is usable rn, come back later!!

## show me the code

```lisp
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
```

available built-ins:

```lisp
add (a : int | float) (b : int | float)
sub (a : int | float) (b : int | float)
mult (a : int | float) (b : int | float)
div (a : int | float) (b : int | float)
mod (a : int | float) (b : int | float)

eq (a : int | float | str | nil) (b : int | float | str | nil)
neq (a : int | float | str | nil) (b : int | float | str | nil)
gt (a : int | float) (b : int | float)
lt (a : int | float) (b : int | float)
ge (a : int | float) (b : int | float)
le (a : int | float) (b : int | float)

if (cond : bool) ( truthy : expr ) ( falsy : expr )
fun (args : symbol list) (body : expr)
let (name : symbol) (value : expr) (nest : expr) ; `name` is now in env when evaluating nest

and (cond : bool) (cond : bool)
not (cond : bool)
or (cond : bool) (cond : bool)

int (x : float | string)
float (x : int | string)
str (x : nil | bool | int | float | string)

cat (a : str) (b : str)

print (x : nil | bool | int | float | string)
readline (prompt : string)

list ...(expr: expr) ; combines any number of expressions into one list type
```

with more to come

## usage

```sh
dune exec clispy # repl
dune exec clispy -- <filename>
```

## todo

- [ ] core language (interpreted)
  - [x] lexer
  - [x] parser
  - [x] interpreter
  - [ ] language features
  - [ ] standard library
  - [ ] improvements (cool and usable error logs, remove weird cases in lexer/parser)
- [ ] beam-like runtime
- [ ] integration
