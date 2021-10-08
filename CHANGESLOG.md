#CHANGELOG
10/5 2021

- remove all index selection in repl and server/database
- fix tag error in tsv2bin
- remove intern symbol in tsv2bin
- disable IO for non-canonical index
- not produce empty file any more
- remove `.size` file read in backend/daemon/compiler, directly calculate file size
- remove `manifest` direct put relation tag in file name
- new relation datafile specification `{tag}.{relation name}.{arity}.table`
