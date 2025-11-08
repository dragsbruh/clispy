# clispy

lisp-like language for a beam-like runtime
neither the language nor the runtime is usable rn, come back later!!

## show me the code

```lisp
(if (lt (mod 7.5 2.3) 1.0)
    (add 3.5 2.1)
    (sub 10.0 4.4))

(add
    (mult 5 (add 3.0 2.0))
    (div 20 4))
```

available built-ins:

```lisp
add (a : int | float) (b : int | float)
sub (a : int | float) (b : int | float)
mult (a : int | float) (b : int | float)
div (a : int | float) (b : int | float)
mod (a : int | float) (b : int | float)
gt (a : int | float) (b : int | float)
lt (a : int | float) (b : int | float)
eq (a : int | float) (b : int | float)
neq (a : int | float) (b : int | float)
ge (a : int | float) (b : int | float)
le (a : int | float) (b : int | float)
if (cond : bool) ( truthy : expr ) ( falsy : expr )
not (cond : bool)
and (cond : bool) (cond : bool)
or (cond : bool) (cond : bool)
int (x : float)
float (x : int)
fun (args : symbol list) (body : expr)
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
