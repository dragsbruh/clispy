# clispy

lisp-like language for a beam-like runtime

the runtime is not usable rn, come back later!!

## show me the code

```lisp
(rec fact (n)
  (if (le n 1)
    1
    (mult n (fact (sub n 1))))
  (print
    (cat
      (cat "the factorial of 10 is " (str (fact 10)))
      "\n")))

(print
  (cat
    (cat
      "the number 10, relative to 5 is "
      (let x 10
        (if (gt x 5)
          "big"
          "small")))
    "\n"))

(rec fib (n)
  (if (le n 1)
    n
    (add (fib (sub n 1)) (fib (sub n 2))))
  (fib 10))

(rec even (n)
  (rec odd (n)
    (if (eq n 0)
      false
      (even (sub n 1)))
    (if (eq n 0)
      true
      (odd (sub n 1))))
  (even 5))
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
rec (name : symbol) (args : symbol list) (body : expr) (nest : expr) ; same as `(let name (fun (...args)) ...)` but recursion is possible

and (cond : bool) (cond : bool)
not (cond : bool)
or (cond : bool) (cond : bool)

int (x : float | string)
float (x : int | string)
str (x : nil | bool | int | float | string)

cat (a : str) (b : str)

print (x : nil | bool | int | float | string) ; does not add newline, might be confusing to use in repl since returned value `nil` is printed
readline (prompt : string)

list ...(expr : expr) ; combines any number of expressions into one list type

sqrt (x : float)
sin (x : float)
cos (x : float)
tan (x : float)
asin (x : float)
acos (x : float)
atan (x : float)
sinh (x : float)
cosh (x : float)
tanh (x : float)
atan2 (x : float) (y : float)

pow (x : float) (pow : float)
exp (x : float)
deg (x : float)
rad (x : float)

pi
epsilon
```

with more to come

## mascot

clispy's mascot is clippy!

![clippy would never screw you over](./images/clippy.png)

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
  - [x] language features
  - [ ] standard library
  - [ ] improvements (cool and usable error logs, remove weird cases in lexer/parser)
- [ ] beam-like runtime
- [ ] integration
