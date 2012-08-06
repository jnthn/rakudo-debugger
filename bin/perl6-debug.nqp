use Perl6::Grammar;
use Perl6::Actions;
use Perl6::Compiler;

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
    $comp.parsegrammar(Perl6::Grammar);
    $comp.parseactions(Perl6::Actions);
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

    # Enter the compiler.
    $comp.command_line(@ARGS, :encoding('utf8'), :transcode('ascii iso-8859-1'));
    
    # Run any END blocks before exiting.
    for @*END_PHASERS { $_() }
}
