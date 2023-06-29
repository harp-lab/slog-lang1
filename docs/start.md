# Getting Started
State-of-the-art Datalog engines include expressive features such as ADTs (structured heap values), stratified aggregation and negation, various primitive operations, and the opportunity for further extension using FFIs. Current parallelization approaches for state-of-art Datalogs target shared-memory locking data-structures using conventional multi-threading, or use the map-reduce model for distributed computing. Furthermore, current state-of-art approaches cannot scale to formal systems which pervasively manipulate structured data due to their lack of indexing for structured data stored in the heap.
    
With slog, we devised a new approach to data-parallel structured deduction that involves a key semantic extension of Datalog to permit first-class facts and higher-order relations via defunctionalization, an implementation approach that enables parallelism uniformly both across sets of disjoint facts and over individual facts with nested structure.

Our approach has three main parts: 
1. A key semantic extension to Datalog, subfacts and a subfact-closure property, that is 
2. Implemented uniformly via ubiquitous fact interning, supported within relational algebra operations
that are 
3. Designed from the ground-up to automatically balance their workload across available
threads, using MPI to address the available data-parallelism directly. 

The Slog language project is led by [The HARP lab](https://github.com/harp-lab) which is led by Dr. Thomas Gilray & Dr. Sidharth Kumar @ The University of Alabama at Birmingham and Dr. Kristopher Micinski @ Syracruse University. We have written a paper introducing our motivations for Slog to the world [here](https://arxiv.org/abs/2211.11573)

## Slog's Design Philosophy
1. <strong>First-Class Facts</strong>: Slog extends Datalog by allowing structurally recursive, first-class facts. This minor semantic change enables both enhanced expressivity and anticipates compilation to parallel relational algebra.

2. <strong>Subfact Closure</strong>: Each structurally unique fact/subfact in Slog has a unique intern-id stored in its 0 column so it may be referenced as another fact’s subfact and treated as a first-class value. All data is at once a first-class fact, a first-class value, and a first-class thread of execution.

3. <strong>Efficient Execution</strong>: Slog's design allows declarative rules to be efficiently translated to efficient execution on modern computer hardware, including multi-core computers.

4. <strong>Expressiveness</strong>: Slog's domain-specific language extensions support the tool designer to structure projects effectively and give sufficient expressiveness to the users.

## Applications of Slog
Slog's approach to data-parallel structured deduction makes it an ideal language for complex data analysis, logic programming, static analysis and much more. 

## Getting started
- Link install .md here