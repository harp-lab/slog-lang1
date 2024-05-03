## Tutorial


To Learn more about how slog works internally and for a more substantial explanation, please refer to [Slog Paper on arXiv](https://arxiv.org/abs/2211.11573)

In here, We will look at some examples involving basic syntax, arithmetic functor's and aggregators at a surface level to get you started writing programs in slog.

This article assumes you have [Set Up Slog](./setup_slog.md), and have gone through [A Simple Example](./TC.md).

We will introduce features and syntax through different examples,

---------------------------------------------------------------------------
### Free Vars
Finding free vars in a lambda expression using slog, for example :
```lambda-cal
(lam (y) (lam (x) (y (z x))))
```
- First, we need to represent the λ expression in slog, we can as below using nested facts:
```slog
(lam "y" (lam "x" (app (ref "y") (app (ref "z") (ref "x")))))
```
- If you run slog with just the fact above,  you will notice that there are three different relations, `lam`, `app` and `ref` generated from this fact. Slog's support for first class facts helped us model λ expression more naturally.
- The program to get the free-var in the expression has the following three rules:
```slog
[(free x (ref x)) <-- (ref x)]
```
- For every `ref` tuple, create a `free` tuple, in isolation every ref fact has a free-var.
- we can also write the above rule as:
```slog
(free x ?(ref x))
```
- `?` is syntactic sugar, and the above rule gets desugared to the prior rule, and next two rules use this convenience to make rules easier to write.
```slog
[(free x ?(lam y eb)) <-- (free x eb) (=/= x y)]

[(free x ?(app ef ea)) <-- (or (free x ef) (free x ea))]
```
- The above two rules, introduce two new features, `or` and `=/=`. `or` is used for dis-junction in the body. `=/=` is used to express `not equal to` operator.
- Running the program with the fact and the above rules, will give you the free vars i.e., `z` in this case.

-------------------------------------

### Same Generation
- This example is taken from [Souffle's tutorial](https://souffle-lang.github.io/tutorial#same-generation-example), and follows the same pattern.
- Given a tree; directed acyclic graph, Same generation is to find nodes that are on the same level in the tree.
```slog
(parent "d" "b") 
(parent "e" "b") 
(parent "f" "c")
(parent "g" "c") 
(parent "b" "a") 
(parent "c" "a")

[(person x) (person y) <-- (parent x y)]

[(samegen x x) <-- (person x)]

[(samegen x y) <-- (parent x p) (samegen p q) (parent y q)]
```
- Here the first rule, has multiple heads, which is a way to express conjunction in the head. It can also be written as: 
```
[(person x) <-- (parent x _)]
[(person x) <-- (parent _ x)]
```
- In the above rules \_ matches with any value, is a wildcard, Souffle has the same syntax.
-----------------------------------------
### Extending TC
#### SCC
- Extending on our [TC](./TC.md) example to compute which nodes are strongly connected.
```slog
; TC
[(path x y) <-- (edge x y)]
[(path x y) <-- (edge x z) (path z y)]

; strongly connected components
[(scc x y) <-- (path x y) (path y x)]
```
- Here `scc` holds all the edges that are strongly connected, an example graph and its scc is shown below
![scc](./images/scc.png)
- Here, the nodes `1,2,3,4` are strongly connected.
#### Cyclic?
- Check whether a node is part of a cycle, add this rules to the TC
```slog
[(cyclic x) <-- (path x x)]
```
#### Acyclic?
- Does graph have any cycles?
```
[(acylic)  <-- ~(cyclic _)]
```
- If none of the nodes are cyclic, the graph is acyclic
- Slog supports Negation, `~` is used to express negation.
-------------------------------
### Arithmetic Functors
- The following examples show how arithmetic operations can be done in slog.
- Slog supports `+, -, *, /`, simplest examples would be to just directly use them.
```slog
(plus {+ 1 1}) ; Expression in curly braces is evaluated first, and fact is generated
[(plus {+ acc 1}) <-- (plus acc) (< acc 5)]

(minus {- 0 1})
[(minus {- acc 1}) <-- (minus acc) (> acc -5)]

(multiply {* 1 2} 1)
[(multiply {* acc 2} {+ 1 power}) <-- (multiply acc power) (<= power 5)]

(divide {/ 128 1})
[(divide {/ acc 2}) <-- (divide acc)]
```
- The arithmetic ops in curly braces are the first operation performed.
- They can also be written without curly braces, using more traditional syntax.
```slog
[(+ 1 1 val) --> (plus val)]
[(plus acc) (< acc 5) (+ acc 1 val) --> (plus val)]
```
- All the arith ops in the main example can be written in this form.
- Please note the usage of comparison operators, Slog supports `<, >, <=, >=, =, =/=`
#### Factorial
- Here is factorial(n) written in slog, try it as an exercise before proceeding.
```slog
[(fact {- n 1} {* acc n}) <-- (fact n acc) (> n 0)]
(fact 5 1)

[(factorial n) <-- (fact x n) (= x 0)]
```
- The first rule is recursive, computes and stores every step of factorial as a tuple in fact relation. The `(fact 5 1)`, triggers the first rule.
- The second rule, just pulls out the final factorial value from the fact relation.
----------------------------------------
### Aggregators
- Slog supports `count, minimum, maximum, sum` ,Slog aggregators are similar to aggregators in SQl; they are used to aggregate values across a relation. 
#### Count
```slog
;; Count
(edge 1 2) (edge 1 4) (edge 2 3) (edge 2 4) (edge 2 5) (edge 2 6) (edge 3 3) (edge 3 4) (edge 5 3)

;; Add the all the nodes with outgoing edges to node relation
[(edge x y) --> (node x)]

;; For each node, count the number of edges outgoing from it and accumulate in degree
[(node x) (count edge x _ degree) --> (node-degree x degree)]

;; below rule, does the same as above two rules combined
;; matches on outgoing node in edge and uses that in count to get degree
[(edge x _) (count edge x _ degree) --> (node-degree-edge x degree)]

;; Another example of using count, this rules just counts the number of edges in edge relation
[(count edge _ _ degree) --> (edge-count degree)]

```

#### Sum
```slog
;; sum
(b-foo 1 1) (b-foo 1 2) (b-foo 1 3)
(b-foo 2 3)

[(sum b-foo _ S) --> (b-foo-sum-1 S)]
[(b-foo x _) (sum b-foo x S) --> (b-foo-sum-2 x S)]
[(b-foo x _) (> x 1) (sum b-foo x S) --> (b-foo-sum-3 x S)]
```
- Here `b-foo` relation has two columns.
- First rule, sums all the values in the second column.
- Second rule, sums all the second column values for each first column value.
- Third rule, sums all the second columns values for each first column value, where first column value is greater than 1.
- Syntax is similar to count.
#### Maximum and Minimum
```slog
;; maximum && minimum
(c-foo "a" 1) (c-foo "a" 10) (c-foo "a" 100) (c-foo "a" 1000)
(c-foo "b" 2) (c-foo "b" 20) (c-foo "b" 200) (c-foo "b" 2000)

[(c-foo x _) (maximum c-foo x max_col2) --> (c-foo-max x max_col2)]
[(c-foo x _) (minimum c-foo x min_col2) --> (c-foo-min x min_col2)]
```
- Again, the syntax is similar to sum and count.
- Here we have `c-foo` relation with two columns, we want the maximum/minimum for the second column, for each distinct first column value.
#### SSSP - Min dist
- Using the aggregators, we can calculate Single Source Shortest Path distance for a node in the graph.
```slog
; (edge src dst weight)
(edge 1 2 1) (edge 2 3 2) (edge 2 4 1)
(edge 3 5 2) (edge 4 5 1) (edge 5 6 2)

; (path dst-node weight)
; we will pick 1 as the src node, and set the weight to itself as 0
(path 1 0)

; this rules gets the distance from 1 to each node
[(path x costx) (edge x y weightxy) --> (path y {+ costx weightxy})]

; Have to filter path and get the minimum weight nodes
[(path x _) (minimum path x min_dist) --> (sssp x min_dist)]
```
- First we accumulate the distances to all nodes from src across different paths.
- Final rule, we use minimum aggregator to find the shortest distance between the source and each node.
---------------------------------------
### Lists and usage
- The free vars example introduced the concept of nested facts, which is a powerful feature in slog. It can be used to represent lists.
```slog
(lst 1 (lst 2 (lst 3 (lst 4 (nil )))))
```
- The above is a valid fact in slog and is basically a singly linked list.
- Once we have list, we need to be able to do some operations on the list, The following examples will show how you can write `length`, `member`, `append`, `reverse` etc.
- In these operations example, we will also introduce `!` form, which lets us recursively generate facts from a rule, before evaluating the surrounding fact, much like a function call.
- Please pay attention to the use of `{}` ,`!` and nested facts in the examples.
#### Length
- The basic idea is to take the outer list, unwrap each element, increase the length by one, and repeat this till we see a `(nil )` fact.
- Here is the slog program to calculate the length of a slog-list.
```slog
(input-list (list 1 (list 2 (list 3 (nil)))))

(length ?(do-length (nil)) 0)

[(length ?(do-length (list hd rst)) {+ len-rst 1}) 
    <-- (length !(do-length rst) len-rst)]

[(print-length-res res)
    <-- (length !(do-length {input-list}) res)]
```
- The above rules can be writtten more explicitly removing the syntactic form `?`, changing the direction of arrow in the rules and reordering the rules. 
- Note, Slog doesn't enforce a order of head and body in a rule, can `head <-- body` or `body --> head`, they are equivalent. And the order of rules in the program doesn't affect the output.
```slog
;  list operation
(input-list (list 1 (list 2 (list 3 (nil)))))

[(length !(do-length {input-list}) res) 
    --> (print-length-res res)]

[(do-length (list hd rst)) (length !(do-length rst) len-rst) 
    --> (length (do-length (list hd rst)) {+ len-rst 1})]

[(do-length (nil)) 
    --> (length (do-length (nil)) 0)]
```
- The first line is the input-list fact, the list is in a relation named input-list, it has three elements `'(1 2 3)`, `(nil)` acts as a marker for end of the list.
- First Rule:
	- Body has a `length`, fact that has `do-length` marked with `!` -- `bang` from hereon -- which has `input-list` in curly braces, and `res` at the end.
	- At this point, the DB doesn't have any `length` facts, but how bang works is, before the surrounding rule is executed, `do-length` fact gets added to the DB, and it triggers some - In this case, second rule gets triggered - rules, once those rules are done, `do-length` will "return", then it's surrounding rule is evaluated.
	- It can thought of as a function call, when `do-length` returns, there will be length facts in the database, and the surrounding rule goes through.
	- One more item here, Curly braces wrapping the relation name `input-list`, expression inside `{}` gets executed first, in this case, `input-list` is replaced with tuples inside the relation, in this case only one list, there can be more lists in the input-list relation.
	- Like `(input-list (list 4 (list 5 (list 6 (list 7 (nil))))))`, and do length would be triggered on both lists.
- Second Rule:
	- Once `(do-length (list 1 (list 2 (list 3 (nil)))))` is in the database, second rule is triggered.
	- In the body of the second rule, we see something similar to first rule, except `do-length` is being "called" for rest - cdr - of the list, and when it returns head has `{+ len-rst 1}`, which emits length fact with increased length.
	- This recurses till `(do-length (nil))` is reached, in which case third rules gets triggered emitting the first `length` fact with `len-rst` as zero.
    - Length fact, as it goes up the recursion, satisfies the surrounding rule it was triggered by, and that rules emits a length fact with increased length.
- That example introduces `!` form, and working with nested facts. 

#### Member
```slog
(member ?(do-member x (nil)) (false))
(member ?(do-member x (list x rst)) (true))
[(member ?(do-member x (list hd rst)) (true)) 
    <-- (=/= hd x) (member !(do-member x rst) (true))]
[(member ?(do-member x (list hd rst)) (false)) 
    <-- (=/= hd x) (member !(do-member x rst) (false))]
[(print-member-res res) <--
	(member !(do-member 2 {input-list}) res)] 
;; 2 in the line above is element we are looking for
```
- This takes a `input-list` like length does, and unwraps the list, and at each unwrapping uses `!` to generate do-member for the rest of the body. The top rules are the base cases.
- First one, if the list is `(nil)`, element is not found, and emits `member` fact with false.
- Second one, if the current head matches the element, emits a `member` fact with true.
- 3rd and 4th rules, are pick up `true` and `false` cases respectively.
- The use of `!` and `{}` is identical to the `length` program.
#### Append an element
```slog
(append ?(do-append (nil) x) (list x (nil)))
[(append ?(do-append (list hd tail) x) (list hd tail-appended)) 
    <-- (append !(do-append tail x) tail-appended)]

[(print-append-res list) <-- (append !(do-append {input-list} 4) list)]
```
- Takes an input list and append an element to it.
- Inspect the `print-append-res` and `append` relation in the REPL to better understanding how append works.
#### Reverse a list
```slog
(reverse (do-reverse (nil)) (nil))
[(reverse ?(do-reverse (list hd tail)) {append !(do-append r hd)}) 
    <-- (reverse !(do-reverse tail) r)]
[(print-reverse-res list) 
    <-- (reverse !(do-reverse {input-list}) list)]
```
- This uses append, Takes each element from the head and adds into a list, giving the reverse of a list.
#### SSSP - Path
- In these example, lists are used to extend our SSSP example - computes min-dist to all the nodes from a source node - to calculate the paths to take for min-dist.
```slog
;; Single source shortest path
;; cyclic
(edge 1 2 1) (edge 3 2 2) (edge 2 4 1)
(edge 5 3 2) (edge 4 5 1) (edge 5 6 2)
(source-node 1)

[(edge from to w) --> (path from to (list to (list from (nil ))) w)]
[(path from mid pt acc-w) (edge mid to w) (member !(do-member to pt) (false))
    --> (path from to (list to pt) {+ acc-w w})]
[(path from to _ _) (source-node from) 
	--> (shortest-dist from to {minimum path from to _})]
[(shortest-dist from to dist) (path from to pt dist) 
	--> (shortest-path from to pt dist)]
```
- This is an interesting program to look at and understand, this uses most of the features the other examples have introduced till this point.
- The example needs the rules from [member](#member) example, they are emitted for brevity.
- The only difference here from the previous SSSP example is, for every hop, along with min-dist calculation, the `to` node is added to a list of nodes in that path given it isn't already present in the path, this membership check is neccessary to avoid infinite loops in cyclic graphs.  

### Sources 
- Some Examples are picked from sources written by [Sun Yihao](https://github.com/StarGazerM), [Akshar Patel](https://github.com/akshar2020)
- [Slog Paper on arXiv](https://arxiv.org/abs/2211.11573) 
- [Souffle's tutorial](https://souffle-lang.github.io/tutorial#same-generation-example)
