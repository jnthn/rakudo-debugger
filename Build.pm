use Panda::Common;
use Panda::Builder;

my $nqp        = 'nqp';
my $parrot     = 'parrot';
my $pbc_to_exe = 'pbc_to_exe';
my $executable = $*OS eq 'MSWin32' ?? 'perl6-debug.exe' !! 'perl6-debug';

class Build is Panda::Builder {
    method build(Str $path) {
        shell "$nqp --vmlibs=perl6_group,perl6_ops --target=pir "
            ~ "--output=perl6-debug.pir bin/perl6-debug.nqp";
        shell "$parrot -o perl6-debug.pbc perl6-debug.pir";
        shell "$pbc_to_exe --output=bin/$executable perl6-debug.pbc"
    }
}
