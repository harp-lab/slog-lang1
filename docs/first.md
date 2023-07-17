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
```bash
(foo 1 2) # A fact called "foo" of arity 2
(bar 2 3) # A fact called "bar" of arity 2
(alan 9 0 12 3 4 5) # A fact called "alan" of arity 6
```
Facts can also hold strings such as 
```bash
(foo "string" "another string")
```
## Rules
In slog, you do not need to define a rule prior to using it; The compiler automatically does it for you. Slog also allows rules with the same name to be defined multiple times in-order to get polyvariants of a rule. 
Take the following rule for example:
```bash
[(foo x) 
  <-- 
 (bar x)]
```
This rule contains a `head and body clause`. The 
