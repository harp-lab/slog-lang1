# Your first program

If you are familiar with Datalog languages and already know Souffle, you may find the comparison between Souffle and Slog to be more userful [here](compare.md)
Otherwise, we will walk you through the building blocks of a slog program. 

## Facts
Facts are rules which hold all your conditions, these rules will have a head clause hence every argument will be grounded. Slog uses s-expr style code for fact representation. 
A `fact` takes follows the following syntax:

```bash
(<fact name> args ...)
```
Here are some examples of some facts. 
```scheme
(foo 1 2) ; A fact called "foo" of arity 2
(bar 2 3) ; A fact called "bar" of arity 2
(alan 9 0 12 3 4 5) ; A fact called "alan" of arity 6
```
Facts can also hold strings such as 
```scheme
(foo "string" "another string")
```
## Rules
In slog, you do not need to define a rule prior to using it; The compiler automatically does it for you. Slog also allows rules with the same name to be defined multiple times in-order to get polyvariants of a rule. 
Take the following rule for example:
```scheme
[(foo x) 
  <-- 
 (bar x)]
```
This rule contains a `head and body clause`. The head is always where the arrow point, thus the above program is the exact same as:
```scheme
[(bar x) 
  -->
 (foo x)]
  ```

## Slog by examples
<!-- Needs more fluff -->
#### 0-Arity
```scheme
(foo 1)
(bot)
[(foo a)
 -->
 (bar (bot) a)]
```
and run using `runslog`
```bash
./runslog -R <path to your .slog program> <path to output folder>
```
You will be presented with a `REPL` after compilation, after dumping path
```bash
dump bar
```
you should be greated with an output of 
```scheme
(bar (bot ) 1)
```

#### Transitive closure:
```scheme
[(path x y) <-- (edge x y)]
[(path x z) <-- (path x y) (edge y z)]
```

of which you may call by defining some facts using edge.

```scheme
(edge 1 2)
(edge 2 3)
```

and run using `runslog`
```bash
./runslog -R <path to your .slog program> <path to output folder>
```

You will be presented with a `REPL` after compilation, after dumping path
```bash
dump path
```
you should be greated with an output of 
```scheme
(path 1 2)

(path 1 3)

(path 2 3)
```
#### Fibonnacci
```scheme

(fib ?do(0) 0)
(fib ?do(1) 1)
[(> n 1)
 -->
 (fib ?do(n) {+ {fib !do({- n 1})} {fib !do({- n 2})}})]

(fib2 {fib !do(2)})
(fib3 {fib !do(3)})
(fib4 {fib !do(4)})
(fib5 {fib !do(5)})
(fib10 {fib !do(10)})
```