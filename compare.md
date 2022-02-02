# Comparsion of Slog and Soufflé

This is a document give an overview of slog language and comparing with state-of-art datalog dialect, [soufflé](https://soufflé-lang.github.io/docs.html). Assuming you already know basic usage of souffl langauge, if not please reference to related part in soufflé's document.

Written by @StarGazerM

## Fact

Facts are rule which will hold in all condition, so these rule will not have body clause but only the head clause. Since it is a head clause of datalog program, so every argument must be grounded. Fact representation in slog and soufflé are almost same, only difference is slog use s-expr style code.

**soufflé**

```
foo(1,2).
```

**slog**

```
(foo 1 2)
```

-------------------

## Rule


### Declaration

In soufflé, you will have to declare a rule using `.decl` before you use that rule.

**soufflé**
```
.decl foo(a: number, b: number)

.decl bar(x: symbol)
bar(x) :-
    foo(x, _).
```

In slog, you don't need to define a rule before use it, compiler will automatically do it for you, and please notice that in soufflé one rule name only correspondant to one name. If you already declare bar have 1 arity than you can't declare another bar has 2 arity. However in slog, user can use a name multiple time to get ployvariant of a rule.

**slog**
```
[(bar x) <-- (foo x _)]
(bar 1 "x")
```

## Head and Body clause 

The head clause in soufflé are always on the left of decduction symbol `:-`, but in slog it can be bidirectional depend on the arrow direction you use to connect 2 rule, for example:

**slog**
```
; following rules are equivalant
[(bar x) <-- (foo x _)]
[(foo x _) --> (bar x)]
```


### Wildcard

As we show in previous example both soufflé and slog allow wildcard argument in rule body.

-------------------------

## Join

### k-arity join

Join is most basic feature in datalog, you can join multiple rules together without too effort.

**soufflé**
```
foo(a, b, d) :-
    cond1(a, c1),
    cond2(b, c2),
    cond3(2, d, b).
```

In slog you can have similar code:

**slog**
```
[(foo a b d)
    <--
    (cond1 a 1) --
    (cond2 b 2)
    (cond3 3 d b)]
```

But it must be pointed out that slog is a langauge focus on parallel performacnce, in order to minimize communication cost between each node, we only allow binary join under the hood. Although above code looks like a k-arity join but it will be compiled to code like:

**slog**
```
[(helper-cond32 b d)
    <--
    (cond2 b 2)
    (cond3 3 b d)]

[(foo a b d)
    <--
    (helper-cond32 b d)
    (cond1 a 1)]
```

Slog using `openmpi` as backend for paralization, in current implementation, only *all-to-all* communication is used between each process(also called `rank` in MPI). This means, when a join happen, all tuples in one of relations will be broadcast to an all2all buffer which can be read by every rank.


### Indices


In datalog, indices order and selection is very crucial to join perfromance. Before talking about indice I want to mention some detail about join first.

Both soufflé and slog use btree as intern data structure to store tuples. for a relation like `foo(a, b, c)`
 As we all know, btree is a sorted data structure, each node in btree is a range of tuple with some order. In soufflé, each tuple will be represented as a 3-array, and the order of tuples will be determined by

 ```
 foo1 < foo2 <=> a1 < a2 ? True : b1 < b2 ? True c1 < c2
 ```
If we have a join on column `a`:
```
res(a, b) :-
    bar(a, c),
    foo(a, b).
```

In this join, the pseudo will looks like
```
for a, _ in rel_bar:
    for b in range((a, MIN), (a, MAX)) of rel_foo:
        rel_res.add((a, b))
```
This only need 1 range operation on btree and then iterate on result range. If the index order of relatin is not `foo(a, b)`, but `foo(b, a)` 

```
res(a, b) :-
    bar(a, c),
    foo(b, a).
```

and we still do the same join as before, the compiled psuedo code will become:
```
for a_bar, _ in rel_bar:
    for b, a_foo in rel_foo:
        if a_foo == a_bar: 
            rel_res.add((a, b))
```
This need a whole iteration on relation bar, which is inefficient as it looks like.
In soufflé you must be very careful about this, and in k-arity join, the rule scheduling will be complicated and hard, souffle do have a optimized index selection algorithm, please refer the paper in souffle website.

Indice mechenism in slog is a bit more complex. In slog, each relation will have a few different indices. The indcies defined by user are called `Canonical Index`, slog compiler will also create some non-canonical indices during organization pass which always put joined column first and drop all unused column. For example, previous code will become something semantically similar to:

```
; res_1_2__2 means relation res has 2 arity and using an index selection 1 2
[(res_1_2__2 a b)
    <--
    (bar_1__2 a)
    (foo_2_1__2 b)]
```
Each non-canonical index is a copy of (part of) it original relation, and data transformation from canonical to non-canonical use a special kind of copy operation `acopy`, which means "atomic copy".



### Body clause partition

`Clause partition` is an concept only in slog. As we mentioned before, all join in slog will be compiled into binary join. During real world program, we found the binary join order dominating the performance of slog performance, if then partition of is not good which cause even 100 times memory and running time cost.

**slog**
```
[(res a b c d)
    <--
    (foo a x)
    (bar x b)
    (prr c d)]
```

In above code, we didn't assign the partition for this rule, so in slog compiler this rule will joined undeterministically. One possible partition is, join `prr` and `bar` first then join with `foo`. This partition will cause a intermediate relation `(join-bar-prr c d x b)` which is the cartisian product of relation `bar` and `prr`, if size of these relation grows, this intermediate relation will become an huge performance overhead. Another possible partition is join `foo` and `bar` first, this time the intermediate relation will be `(join-foo-bar a b)` which contain less data then previous join in many cases because it has less columns. But the later join order is not always better, if the relation `foo` size is huge and `bar` is very small, this partition maybe cause more intermediate memory cost.

In one word, programmer must select partition of slog rule very careful with repsect to both how intermediate relation looks like and the size of relation. Since effective automatic clause partition is very hard, slog provide a keyword `--`. Programer can use this to manually specify clause partition in following way.

**slog**
```
; join bar and prr first
[(res a b c d)
    <--
    (foo a x) --
    (bar x b)
    (prr c d)]

; join bar and foo then prr then gaa
[(res a b c d e)
    <--
    (gaa e)
    --
    (prr c d)
    --
    (foo a x)
    (bar x b)]
```

Another thing need to be mentioned is, the usage of `--` related with the arrow order of rule. Above code shows a example how to use `--` with `<--`, the order is from bottom up. For `-->` all rule will be joined reversely:
```
[(foo a x)
 (bar x b) --
 (prr c d)
    -->
    (res a b c d)]
```

### Disjuction

This is similar in slog and souffle

**souffle**
```
foo(a, b) :-
    bar(a, c)
    (gaa(b, c) ; kaa(b, c)).
```

**slog**
```
[(foo a b)
    <--
    (bar a c)
    (or (gaa b c)
        (kaa b c))]
```


----------------------------------


## Negation and Aggregation

Slog and souffle both support stratified negation. Usage of negation in slog and souffle are almost same. Negation in slog is implemented as a special copy rule, negated rule will be broadcasted to all rank and each rank filter and copy the unmatched data into result relation.

**souffle**

```
foo(a, b) :- bar(a, b), !booo(a, b).
```

**slog**
```
[(foo a b c) <-- (~ boo a b) -- (gaa b c) (bar a b)]

; this partition is invalid in slog
; [(foo a b c) <-- (bar a b) -- (gaa b c) (~ boo a b)]
```
Due to the nature of stratified negation, all arguments in it need to be grounded, so as in generated intermediate relation, so when use negation is large join user need to be more careful of partition selection.

Aggregation support in slog is on roadmap.


------------------------------


## First class relation

One most difference between slog and souffle is, slog support first class relation which means you can use relation as value and stored it just as integer and string.

MORE....

------------------------------

## Primitive, builtin and computation relation

There are 2 different type of value is souffle `symbol` (actually is string) and `number` (include floating number). And you apply builtin operated (even custom through c++ interface) on these primitive values.

**souffle**
```
; arith
foo(a+b) :- bar(a, b).

; concat string
Y("a","b").
Y("c","d").
Z(a,b, cat(cat(a,b), a)) :- Y(a,b).
```

In slog, current implement allow a lot of builtin for number, but not for string, becasue in tuple format, string will be computed as its hash value, and slog compiler/daemon process will create a intern string file `$strings.csv` for each database, in this csv file will store all used string and its hash value. In future we maybe want to change string to fully materialized string (we can implement some constrain operator which won't generate new string).

The builtin in slog is implemented very different from souffle, we introduce an concept called `computational relation`. Generally speaking, this is similar to some inlined relation in souffle, the value of these relation are only avaliable when computation actually happen. One benefit user can get from  `Computational relation` is it allow user use exist rule to create their own builtin, for example:

**slog**
```
(comp abs)
[(< x 0) (* x -1 -x) --> (abs x -x)]
[(>= x 0) --> (abs x x)]
```

In order to use binary operator like `+-*/` easier(like souffle), slog provide a syntax surgar `{}`. . This make the join of last argument in computation relation same as fetching return value in functional programming language. Below is the rule rewritten using curly brace.

**slog**
```
[(foo {+ a b}) <-- (bar a b)]

[(< x 0) --> (abs x {* x -1})]
[(> x 0) --> (abs x x)]
[(foo {abs x}) <-- (bar x)]
```

------------------------------

## Type system

soufflé have type system which can let you restrict the value type of each column.

**soufflé**
```
.decl A(x:number, y:symbol)

// A("a", "b") 
// compilation error
```

Currently, slog can be considered as a dynamic type language, data in same column of a relation doen't need to be some type, no type check in each join, so you can can have:

**slog**
```
[(foo "a") <-- (bar "a")]
[(foo 1) <-- (bar 1)]
```


## 

## Data structure switch

Both soufflé and slog using `btree`, in soufflé, you can have a optional syntax to switch to data structure `brie` (a trie like hybrid data structure) and `eqrel` (a data structure for transitive equality).

**soufflé**
```
.decl A(x:number, y:number) btree
.decl B(xS:number, y:number) brie
.decl C(x:number, y:number) eqrel
```
