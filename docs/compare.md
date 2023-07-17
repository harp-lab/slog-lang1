# Comparison of Slog and Soufflé

This is a document giving an overview of slog language and comparing with state-of-art datalog dialect, [soufflé](https://soufflé-lang.github.io/docs.html). Assuming you already know basic usage of soufflé language, if not please reference a related part in soufflé's document.

Written by @StarGazerM

## Fact

Facts are rules which will hold in all conditions, so these rules will not have a body clause but only the head clause. Since it is a head clause of a datalog program, every argument must be grounded. Fact representation in slog and soufflé are almost same, only difference is slog uses s-expr style code.

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

In slog, you don't need to define a rule before using it, the compiler will automatically do it for you, and please notice that in soufflé one rule name only corresponds to one name. If you already declare bar have 1 arity than you can't declare another bar has 2 arity. However in slog, users can use a name multiple times to get  polyvariants of a rule.

**slog**
```
[(bar x) <-- (foo x _)]
(bar 1 "x")
```

## Head and Body clause

The head clause in soufflé are always on the left of deduction symbol `:-`, but in slog it can be bidirectional depend on the arrow direction you use to connect 2 rule, for example:

**slog**
```
; following rules are equivalent
[(bar x) <-- (foo x _)]
[(foo x _) --> (bar x)]
```


### Wildcard

As we show in previous examples both soufflé and slog allow wildcard arguments in the rule body.

-------------------------

## Join

### k-arity join

Join is the most basic feature in datalog, you can join multiple rules together without too much effort.

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

But it must be pointed out that slog is a language focused on parallel performance, in order to minimize communication cost between each node, we only allow binary join under the hood. Although above code looks like a k-arity join but it will be compiled to code like:

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

Slog using `openmpi` as backend for parallelization, in current implementation, only *all-to-all* communication is used between each process(also called `rank` in MPI). This means, when a join happens, all tuples in one of the relations will be broadcast to an all2all buffer which can be read by every rank.


### Indices


In datalog, indices order and selection is very crucial to join performance. Before talking about indices I want to mention some details about joining first.

Both soufflé and slog use btree as intern data structure to store tuples. for a relation like `foo(a, b, c)`
 As we all know, btree is a sorted data structure, each node in btree is a range of tuples with some order. In soufflé, each tuple will be represented as a 3-array, and the order of tuples will be determined by

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
This only need 1 range operation on btree and then iterate on result range. If the index order of relation is not `foo(a, b)`, but `foo(b, a)`

```
res(a, b) :-
    bar(a, c),
    foo(b, a).
```

and we still do the same join as before, the compiled pseudo code will become:
```
for a_bar, _ in rel_bar:
    for b, a_foo in rel_foo:
        if a_foo == a_bar:
            rel_res.add((a, b))
```
This needs a whole iteration on the relation bar, which is inefficient as it looks like.
In soufflé you must be very careful about this, and in k-arity join, the rule scheduling will be complicated and hard, soufflé does have an optimized index selection algorithm, please refer to the paper in soufflé website.

Indice mechanism in slog is a bit more complex. In slog, each relation will have a few different indices. The indices defined by the user are called `Canonical Index`, the slog compiler will also create some non-canonical indices during organization pass which always put the joined column first and drop all unused columns. For example, previous code will become something semantically similar to:

```
; res_1_2__2 means relation res has 2 arity and using an index selection 1 2
[(res_1_2__2 a b)
    <--
    (bar_1__2 a)
    (foo_2_1__2 b)]
```
Each non-canonical index is a copy of (part of) its original relation, and data transformation from canonical to non-canonical uses a special kind of copy operation called `acopy`, which means "atomic copy".



### Body clause partition

`Clause partition` is a concept only in slog. As we mentioned before, all joins in slog will be compiled into binary join. During a real world program, we found the binary join order dominating the performance of slog performance, if the partition of the program is not good which may cause even 100 times memory and running time cost.

**slog**
```
[(res a b c d)
    <--
    (foo a x)
    (bar x b)
    (prr c d)]
```

In the above code, we didn't assign the partition for this rule, so in the slog compiler this rule will join non deterministically. One possible partition is, join `prr` and `bar` first then join with `foo`. This partition will cause an intermediate relation `(join-bar-prr c d x b)` which is the cartesian product of relation `bar` and `prr`, if size of these relation grows, this intermediate relation will become a huge performance overhead. Another possible partition is join `foo` and `bar` first, this time the intermediate relation will be `(join-foo-bar a b)` which contain less data then previous join in many cases because it has less columns. But the later join order is not always better, if the relation `foo` size is huge and `bar` is very small, this partition maybe cause more intermediate memory cost.

In one word, a programmer must select the partition of slog rule very carefully with respect to both how intermediate relation looks like and the size of relation. Since effective automatic clause partition is very hard, slog provides a keyword `--`. Programmers can use this to manually specify clause partitions in the following way.

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

Another thing that needs to be mentioned is the usage of `--` related with the arrow order of rule. Above code shows an example of how to use `--` with `<--`, the order is from bottom up. For `-->` all rule will be joined reversely:
```
[(foo a x)
 (bar x b) --
 (prr c d)
    -->
    (res a b c d)]
```

### Disjuction

This is similar in slog and soufflé

**soufflé**
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

Slog and soufflé both support stratified negation. Usage of negation in slog and soufflé are almost the same. Negation in slog is implemented as a special copy rule, negated rule will be broadcasted to all rank and each rank filter and copy the unmatched data into result relation.

**soufflé**

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

Aggregation support in slog is on the roadmap.


------------------------------


## First class relation

One most difference between slog and soufflé is, slog supports first class relation which means you can use relation as value and stored it just as integer and string.

MORE....

------------------------------

## Primitive, builtin and computation relation

There are 2 different types of value in soufflé: `symbol` (actually is string) and `number` (including floating number). And you apply builtin operations (even custom through c++ interface) on these primitive values.

**soufflé**
```
; arith
foo(a+b) :- bar(a, b).

; concat string
Y("a","b").
Y("c","d").
Z(a,b, cat(cat(a,b), a)) :- Y(a,b).
```

In slog, current implement allow a lot of builtin for number, but not for string, because in tuple format, string will be computed as its hash value, and slog compiler/daemon process will create a intern string file `$strings.csv` for each database, in this csv file will store all used string and its hash value. In the future we may want to change the string to a fully materialized string (we can implement some constraint operator which won't generate new strings).

The builtin in slog is implemented very differently from soufflé, we introduce a concept called `computational relation`. Generally speaking, this is similar to some inlined relation in soufflé, the value of these relations are only available when computation actually happens. One benefit user can get from  `Computational relation` is it allow user use exist rule to create their own builtin, for example:

**slog**
```
(comp abs)
[(< x 0) (* x -1 -x) --> (abs x -x)]
[(>= x 0) --> (abs x x)]
```

In order to use binary operator like `+-*/` easier(like soufflé), slog provides a syntax sugar `{}`. . This makes the join of the last argument in computation relation the same as fetching return value in functional programming language. Below is the rule rewritten using curly brace.

**slog**
```
[(foo {+ a b}) <-- (bar a b)]

[(< x 0) --> (abs x {* x -1})]
[(> x 0) --> (abs x x)]
[(foo {abs x}) <-- (bar x)]
```

------------------------------

## Type system

soufflé has a type system which can let you restrict the value type of each column.

**soufflé**
```
.decl A(x:number, y:symbol)

// A("a", "b")
// compilation error
```

Currently, slog can be considered as a dynamic type language, data in same column of a relation does not need to be some type, no type check in each join, so you can can have:

**slog**
```
[(foo "a") <-- (bar "a")]
[(foo 1) <-- (bar 1)]
```


##

## Data structure switch

Both soufflé and slog using `btree`, in soufflé, you can have an optional syntax to switch to data structure `brie` (a trie like hybrid data structure) and `eqrel` (a data structure for transitive equality).

**soufflé**
```
.decl A(x:number, y:number) btree
.decl B(xS:number, y:number) brie
.decl C(x:number, y:number) eqrel
```


