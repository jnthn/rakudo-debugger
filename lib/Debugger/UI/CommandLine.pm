module Debug::UI::CommandLine;

use Term::ANSIColor;

# The source code of the files we've encountred while debugging.
my %sources;

# Represents a file that we're debugging.
my class SourceFile {
    has $.filename;
    has $.source;
    has @!lines;
    has @!line_offsets;
    
    method BUILD(:$!filename, :$!source) {
        # Store (abbreviated if needed) lines.
        @!lines = lines($!source).map(-> $l {
            $l.chars > 77 ?? $l.substr(0, 74) ~ '...' !! $l
        });
        
        # Calculate line offsets.
        for $!source.match(/\N* \r?\n/, :g) -> $m {
            @!line_offsets.push($m.from);
        }
        @!line_offsets.push($!source.chars);
    }
    
    method line_of($pos, $def_line, $def_pos) {
        my $last_p = 0;
        for @!line_offsets.kv -> $l, $p {
            if $p > $pos {
                return ($l - 1, abs($pos - $last_p));
            }
            $last_p = $p;
        }
        return ($def_line, $def_pos)
    }
    
    sub normal_lines(@lines, $color) {
        @lines.map: {
            colored('| ', $color) ~ $_.subst("\r", "")
        }
    }
    
    sub error_lines(@lines) {
        @lines.map: {
            colored('| ' ~ $_.subst("\r", ""), 'red')
        }
    }
    
    sub highlighted_lines(@lines, $start_line_pos, $end_line_pos) {
        @lines.map: {
            state $line = 0;
            NEXT $line++;
            my $safe_start_pos = [min] $start_line_pos, .chars - 1;
            my $safe_end_pos   = [min] $end_line_pos, .chars - 1;
            my $rendered       = colored('| ', 'blue');
            if $line == 0 && $line == @lines.end {
                $rendered ~= .substr(0, $safe_start_pos);
                $rendered ~= colored(
                    .substr($safe_start_pos, $safe_end_pos - $safe_start_pos),
                    'bold yellow');
                $rendered ~= .substr($safe_end_pos);
            }
            elsif $line == 0 {
                $rendered ~= .substr(0, $safe_start_pos);
                $rendered ~= colored(.substr($safe_start_pos), 'bold yellow');
            }
            elsif $line == @lines.end {
                $rendered ~= colored(
                    .substr(0, $safe_end_pos),
                    'bold yellow');
                $rendered ~= .substr($safe_end_pos);
            }
            else {
                $rendered ~= colored($_, 'bold yellow');
            }
            $rendered.subst("\r", "")
        }
    }
    
    method summary_around($from, $to) {
        my ($from_line, $from_pos) = self.line_of($from, 0, 0);
        my ($to_line, $to_pos)     = self.line_of($to, $from_line, $from_pos);
        my $ctx_start = $from_line - 2;
        $ctx_start = 0 if $ctx_start < 0;
        my $ctx_end = $to_line + 2;
        $ctx_end = +@!lines - 1 if $ctx_end >= @!lines;
        return join "\n",
            colored("+ $!filename ($ctx_start - $ctx_end)", 'blue'),
            normal_lines(@!lines[$ctx_start..^$from_line], 'blue'),
            highlighted_lines(@!lines[$from_line..$to_line], $from_pos, $to_pos),
            normal_lines(@!lines[$to_line^..$ctx_end], 'blue');
    }
    
    method exception_summary($e, $line) {
        my $ctx_start = $line - 2;
        $ctx_start = 0 if $ctx_start < 0;
        my $ctx_end = $line + 2;
        $ctx_end = +@!lines - 1 if $ctx_end >= @!lines;
        return join "\n",
            colored("+ Uncaught Exception", 'red'),
            colored('| ', 'red') ~ $e.message,
            colored("+ $!filename ($ctx_start - $ctx_end)", 'red'),
            normal_lines(@!lines[$ctx_start..^$line], 'red'),
            error_lines([@!lines[$line]]),
            normal_lines(@!lines[$line^..$ctx_end], 'red');
    }
}

# Holds the current state of the debugger.
my class DebugState {
    my $dying = False;
    
    method eval_in_ctx($ctx, $code) {
        ENTER $*DEBUG_HOOKS.suspend();
        LEAVE $*DEBUG_HOOKS.unsuspend();
        my $compiler := pir::compreg__PS('perl6');
        my $vm_ctx   := nqp::getattr(nqp::p6decont($ctx), PseudoStash, '$!ctx');
        my $pbc      := $compiler.compile($code, :outer_ctx($vm_ctx), :global(GLOBAL));
        nqp::atpos($pbc, 0).set_outer_ctx($vm_ctx);
        $pbc();
    }
    
    method enter_death_throes() {
        # Or is that the death throws?
        $dying = True;
    }
    
    method issue_prompt($ctx) {
        loop {
            given prompt(colored('> ', $dying ?? 'red' !! 'blue')) {
                when '' {
                    if $dying {
                        say colored(
                            'Cannot continue execution after an unhandled exception',
                            'red');
                    }
                    else {
                        return;
                    }
                }
                when /^ < p print s say > \s+ (.+)/ {
                    say self.eval_in_ctx($ctx, ~$0);
                    CATCH {
                        default {
                            say colored($_.message, 'red');
                        }
                    }
                }
                when /^ < e eval > \s+ (.+)/ {
                    self.eval_in_ctx($ctx, ~$0);
                    CATCH {
                        default {
                            say colored($_.message, 'red');
                        }
                    }
                }
                when /^ (< $ @ % > .+)/ {
                    say self.eval_in_ctx($ctx, ~$0).perl;
                    CATCH {
                        default {
                            say colored($_.message, 'red');
                        }
                    }
                }
                when '?' | 'h' | 'help' {
                    say self.usage()
                }
                when 'q' | 'quit' {
                    exit(0);
                }
                default {
                    say "Sorry, I don't understand"
                }
            }
        }
    }
    
    method usage() {
        join "\n",
            ('<enter>            single step' unless $dying),
            's[ay], p[rint]     evaluate and display an expression in the current scope',
            'e[val]             evaluate an expression in the current scope',
            '$s, @a, %h         show .perl of the a variable in scope (indexing allowed)',
            'q[uit]             exit the debugger'
            ;
    }
}

# Install various hooks.
$*DEBUG_HOOKS.set_hook('new_file', -> $filename, $source {
    say colored('>>> LOADING ', 'magenta') ~ $filename;
    %sources{$filename} = SourceFile.new(:$filename, :$source);
});
$*DEBUG_HOOKS.set_hook('statement_simple', -> $filename, $ctx, $from, $to {
    say %sources{$filename}.summary_around($from, $to);
    DebugState.issue_prompt($ctx);
});
$*DEBUG_HOOKS.set_hook('statement_cond', -> $filename, $ctx, $type, $from, $to {
    say %sources{$filename}.summary_around($from, $to);
    DebugState.issue_prompt($ctx);
});

# Override handler for uncaught exceptions.
my Mu $p6comp := pir::compreg__Ps('perl6');
$p6comp.HOW.find_method($p6comp, 'handle-exception').wrap(-> |$ {
    my Mu $vm_ex := nqp::atpos(pir::perl6_current_args_rpa__P(), 1);
    pir::perl6_invoke_catchhandler__vPP(&unhandled, $vm_ex);
});
sub unhandled(|$) {
    my Mu $vm_ex := nqp::atpos(pir::perl6_current_args_rpa__P(), 0);
    my $e = EXCEPTION($vm_ex);
    my $bt = $e.backtrace();
    my $ctx = CALLER;
    my ($file, $line);
    for @$bt {
        if %sources.exists(.file) {
            $file = .file;
            $line = .line;
            last;
        }
        $ctx = $ctx.WHO.<CALLER>;
    }
    if $file {
        DebugState.enter_death_throes();
        say %sources{$file}.exception_summary($e, $line - 1);
        DebugState.issue_prompt($ctx.WHO);
    }
    else {
        say "Unhandled exception: $e.message() @ $file:$line";
        exit(0);
    }
}
