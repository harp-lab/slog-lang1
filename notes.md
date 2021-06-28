## Databases

- A database is a (handle to) an immutable set of facts, along with
  associated metadata defining relation tags.
- Databases are persisted on disc as directories by writing out sets 
  of fact files along with a standard `manifest` file formatted as 
  an s-expression (in a predicate defined in slog-lang).
- Database manifest must be respected: design system so we avoid need 
  to (manually) convert relation tags on all facts in a table, instead 
  generate code that works with a given manifest.
- Databases may be associated with saved names unique to each user. 
  In other words, Alice and Bob may both use `db0` separately without
  seeing each others' work.

## Authentication

- Every user logs in with a password, and is then given a session key.
- Sessions keys expire N minutes (maybe 180?) after their most recent use.
- Session key sent along with / authenticated for each message.

## Backend / Admin Node

- The REPL and other slog services (e.g., IDE integration into VisualStudio)
  interface with an administrator node that holds the metadatabase for
  all Slog data, including accounts, mappings of databases to their locations
  on disc, etc...
- The administrator node facilitates interaction between a pool of slog 
  workers and REPL sessions via gRPC.
- The administrator also manages a pool of Slog workers and talks to each
  via gRPC messages (probably over a message queue? Or perhaps admin
  maintains a pool of connections).

## `slog-process`

- Admin node compiles sets of hashes using `slog-process`.
- S-expression-based interface to compile files and write to FS.

## Tuple encoding format (u64)

## Compilation units and `#include`

- The administrator manages source files and compiles them on-demand by 
  keeping an open connection to a running (Racket-based) Slog compiler.
- The primitive compilation unit is a (a) source tree (mapping strings, 
  either file paths or hashes) (b) a set of paths to input / output files
  for each relation, and (c) a manifest that defines a mapping between 
  relation-arity tags
- The administrator node interns a set of "hashes," which canonicalize a
  file by hashing it after changing every `(include ...)` line to an 
  empty line.
- When the user runs a file at a REPL, the REPL does a first pass over the
  .slog file to assemble a transitive source tree of `.slog` files, 
  hashes each of them, and exchanges those hashes with the administrator
  node.
- Thus, compilation assembles an output database hash comprised of the
  input database's hash and the canonicalized set of hashes (sorted) of
  the source inputs, which are implicitly `#include`'d together and
  the order of inclusion must not matter.

## REPL/REPL Language

- By default you start out in the bottom database
- Each time you perform a slog operation, the REPL makes a call and gets 
  a new database handle.
- The REPL associates each interaction (that changes the DB) with a 
  "pretty name"
- ** There are two types of running: running a single SCC, and running a 
  sequence of SCCs. There is also a general `run` command which breaks
  apart SCCs into separate hashes(?) and then runs them in sequence.
- For example, `run "foo.slog"` runs `foo.slog` (and its transitively-included
  files) to produce an output db with hash `a31ebb...`, which the REPL then
  associates with `db0`. Subsequent repl commands can then refer to db0.
- There is a "current database" whose pretty name is written to the REPL 
  with each interaction.
- Throughout a REPL interaction, databases are kept "alive," and can be 
  referenced via the pretty names (e.g., `db0`) tracked by the REPL, but 
  databases are marked for garbage collection upon logout (session key
  expiration). The backend does not even track pretty names.
- There is a command `remember db0 "db0"` which takes two arguments: a pretty 
  database name (tracked in the REPL) and a string specifying a persistent 
  path to assocaite the database on the Slog backend. The path specified in 
  strings is interpreted as a path to "slog database path". We may follow 
  a convention like `pldi2022/experiments/batch1` to emulate a filesystem.
- Databases previously computed or saved can be used as arguments to `run`.
  For example, either `run "foo.slog" db0` or `run "foo.slog" "pldi-expts"`
  should work.
- Querying will be supported using an interface like `query (R x (G x x))`
- The REPL builds a synthetic file to add the new rule that contains the 
  following contents:
```
($qry0 ?(R x (G x x)))
```
- The REPL then takes this new file and compiles it, along with the source
  file (and any previous queries) using the manifest from the current
  database. Then the combined file is run, accepting the current database
  as input relations and writing a set of output files.
- Queries are built and run instantaneously.
- Ideally: we need to avoid copying (to disc) relations that are never
  modified when adding an extra query that does not touch any other
  relations (including things such as `::`).
- `emptyrel db0 R0` empties a relation `R0` in database `db0`. 
- `putrel db0 R0 db1 R1` first drops relation `R0` from database `db0`, 
  and builds a new database that replaces `R0` with `R1` in database `db1`.
  The new database will attempt to use the same tag for `R0` as `R1`'s tag
  to avoid having to copy. Produces a new database with the updated manifest.
  **ISSUE** with this and with emptyrel: what about links *to* these facts?
- `assert (P 1 2)` adds a fact to the database as a separate editor buffer in
  the same way as querying a rule. This is an alternative to `putrel` for a 
  single rule.
- `edit` puts the user in editing mode. In editing mode, the user can append 
  new facts and rules to the end of the file. Maybe this will enable a kind of
  printf-style debugging wherein you break apart rules on-the-fly?
  
Complete list of commands:
```
pretty-name ::= db0 | qry0 | ...
db-path ::= " <string> "
db-name ::= pretty-name | db-path
rel-name ::= R | ...

cmd ::=
  run-seq (strata "foo.slog" ...) (strata "foo1.slog" ...) db-name?
  run-scc "foo.slog" db-name?
  run "foo.slog" db-name?
  remember pretty-name db-path
  assert (P 1 2)
  (dump|d) rel-name db-name?
  (query|q) slog-query (R _ (Q x 1)) db-name?
  (load|l) db-name
  putrel db-name rel-name db-name rel-name
```

# Worker / Admin Process
