NQP        = nqp
PARROT     = parrot
PBC_TO_EXE = pbc_to_exe

all: perl6-debug.exe

perl6-debug.exe: perl6-debug.pbc
	$(PBC_TO_EXE) perl6-debug.pbc

perl6-debug.pbc: bin/perl6-debug.nqp
	$(NQP) --vmlibs=perl6_ops --target=pir --output=perl6-debug.pir bin/perl6-debug.nqp
	$(PARROT) -o perl6-debug.pbc perl6-debug.pir
