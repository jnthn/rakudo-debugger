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
        @!lines = lines($!source).map(*.subst("\r", "")).map(-> $l {
            $l.chars > 79 ?? $l.substr(0, 76) ~ '...' !! $l
        });
        
        # Calculate line offsets.
        for $!source.match(/^^ \N+ $$/, :g) -> $m {
            @!line_offsets.push($m.from);
        }
    }
    
    method line_of($pos) {
        for @!line_offsets.kv -> $l, $p {
            return $l - 1 if $p > $pos;
        }
    }
    
    method summary_around($from, $to) {
        my $from_line = self.line_of($from) || 0;
        my $to_line = self.line_of($to) || $from_line;
        if $to_line - $from_line > 5 {
            $to_line = $from_line + 4;
            return colored(join("\n", @!lines[$from_line..$to_line]), 'black on_yellow');
        }
        else {
            my $ctx_start = $from_line - 2;
            $ctx_start = 0 if $ctx_start < 0;
            my $ctx_end = $to_line + 2;
            $ctx_end = +@!lines - 1 if $ctx_end >= @!lines;
            return
                @!lines[$ctx_start..^$from_line].join("\n") ~ "\n" ~
                colored(@!lines[$from_line..$to_line].join("\n"), 'bold yellow') ~ "\n" ~
                @!lines[$to_line^..$ctx_end].join("\n");
        }
    }
}

my class DebugState {
    method eval_in_ctx($ctx, $code) {
        ENTER $*DEBUG_HOOKS.suspend();
        LEAVE $*DEBUG_HOOKS.unsuspend();
        my $compiler := pir::compreg__PS('perl6');
        my $vm_ctx   := nqp::getattr(nqp::p6decont($ctx), PseudoStash, '$!ctx');
        my $pbc      := $compiler.compile($code, :outer_ctx($vm_ctx), :global(GLOBAL));
        nqp::atpos($pbc, 0).set_outer_ctx($vm_ctx);
        $pbc();
    }
    
    method issue_prompt($ctx) {
        loop {
            given prompt("> ") {
                when '' {
                    return;
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
            '<enter>            single step',
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
