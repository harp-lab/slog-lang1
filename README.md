### Slog v1: Data-Parallel Deductive Programming


Slog is a language, compiler, and runtime system for implementing
data-parallel deductive programming.

The best way to understand Slog is to read [our preprint](https://arxiv.org/abs/2211.11573).

## Components

- `compiler/`         -- Compiler from Slog source code to a parallel RA plan
- `backend/`          -- MPI-based parallel relational algebra plan (RA) runtime
- `runslog`           -- Script to build and run slog program with given input data

## A simple example
Here's how you do Transitive Closure in Slog

```datalog
[(path x y) <-- (edge x y)]
[(path x z) <-- (edge x y) (path y z)]
```

The Following guide can help you quickly get started with Slog:

## Getting Started
The below video follows the first two articles -- setting up slog and running TC -- and the first example from the Tutorial article, [![Slog Set-Up and Running Examples](http://i3.ytimg.com/vi/S_cVmsByvHs/hqdefault.jpg)](https://www.youtube.com/watch?v=S_cVmsByvHs)
- [Set Up Slog](./doc/setup_slog.md) 
- [A Simple Example: Transitive Closure](./doc/TC.md)
- [Tutorial](./doc/tutorial.md) 
- [REPL and runslog](./doc/repl_and_runslog.md) 
- [Comparison with Souffle](./doc/compare.md)
- Components and Internals (Todo)

If you have issues or find any mistakes, please raise an issue.


         
