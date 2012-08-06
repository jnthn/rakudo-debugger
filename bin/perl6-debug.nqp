use Perl6::Grammar;
use Perl6::Actions;
use Perl6::Compiler;

class Perl6::DebugHooks {
    has %!hooks;
    
    method set_hook($name, $callback) {
        $*W.add_object($callback);
        %!hooks{$name} := $callback;
    }
    
    method has_hook($name) {
        nqp::existskey(%!hooks, $name)
    }
    
    method get_hook($name) {
        %!hooks{$name}
    }
}

class Perl6::HookGrammar is Perl6::Grammar {
    my %seen_files;
    
    method statementlist() {
        my $file := pir::find_caller_lex__Ps('$?FILES') // '<unknown>';
        unless nqp::existskey(%seen_files, $file) {
            if $*DEBUG_HOOKS.has_hook('new_file') {
                $*DEBUG_HOOKS.get_hook('new_file')($file, self.MATCH.orig);
            }
            %seen_files{$file} := 1;
        }
        Perl6::Grammar.HOW.find_method(Perl6::Grammar, 'statementlist')(self)
    }
}

class Perl6::HookActions is Perl6::Actions {
    method statement($/) {
        Perl6::Actions.statement($/);
        my $stmt := $/.ast;
        if $*DEBUG_HOOKS.has_hook('statement') {
            $/.'!make'(QAST::Stmts.new(
                QAST::Op.new(
                    :op('call'),
                    QAST::WVal.new( :value($*DEBUG_HOOKS.get_hook('statement')) ),
                    $*W.add_string_constant(~$/)
                ),
                $stmt
            ));
        }
    }
}

sub hll-config($config) {
    $config<name>           := 'rakudo';
    $config<version>        := '';
    $config<release-number> := '';
    $config<codename>       := '';
    $config<build-date>     := '2012-08-05T16:57:45Z';
}

sub MAIN(@ARGS) {
    # Initialize dynops.
    pir::rakudo_dynop_setup__v();

    # Bump up Parrot's recursion limit
    pir::getinterp__P().recursion_limit(100000);

    # Create and configure compiler object.
    my $comp := Perl6::Compiler.new();
    $comp.language('perl6');
    $comp.parsegrammar(Perl6::HookGrammar);
    $comp.parseactions(Perl6::HookActions);
    $comp.addstage('syntaxcheck', :before<past>);
    $comp.addstage('optimize', :before<post>);
    hll-config($comp.config);
    my $COMPILER_CONFIG := $comp.config;
    
    # Add extra command line options.
    my @clo := $comp.commandline_options();
    @clo.push('setting=s');
    @clo.push('c');
    @clo.push('I=s');
    @clo.push('M=s');

    # Set up module loading trace
    my @*MODULES := [];
    
    # Set up END block list, which we'll run at exit.
    my @*END_PHASERS := [];
    
    # Force loading of the debugger module.
    my $pname := @ARGS.shift();
    @ARGS.unshift('-Ilib');
    @ARGS.unshift('-MDebugger::UI::CommandLine');
    @ARGS.unshift($pname);
    
    # Set up debug hooks object.
    my $*DEBUG_HOOKS := Perl6::DebugHooks.new();

    # Enter the compiler.
    $comp.command_line(@ARGS, :encoding('utf8'), :transcode('ascii iso-8859-1'));
    
    # Run any END blocks before exiting.
    for @*END_PHASERS { $_() }
}
