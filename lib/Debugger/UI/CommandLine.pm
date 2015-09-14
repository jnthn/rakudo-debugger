unit module Debug::UI::CommandLine;

use nqp;
use Terminal::ANSIColor;
use nqp;

# The source code of the files we've encountred while debugging.
my %sources;

sub eval_in_ctx($ctx, $code) {
    ENTER $*DEBUG_HOOKS.suspend();
    LEAVE $*DEBUG_HOOKS.unsuspend();
    my $compiler := nqp::getcomp('perl6');
    my $vm_ctx   := nqp::getattr(nqp::decont($ctx), PseudoStash, '$!ctx');
    my $comp'd   := nqp::findmethod($compiler, 'compile')($compiler,
                        $code, :outer_ctx($vm_ctx), :global(GLOBAL));
    nqp::forceouterctx($comp'd, $vm_ctx);
    $comp'd();
}

# Represents a file that we're debugging.
my class SourceFile {
    has $.filename;
    has $.source;
    has @!lines;
    has @!line_offsets;
    has @!regex_regions;
    has %!routine_regions{Range};
    
    my class RoutineInfo {
        has $.type;
        has $.name;
    }
    
    method BUILD(:$!filename, :$!source) {
        # Ensure source ends with a newline.
        unless $!source ~~ /\n$/ {
            $!source ~= "\n";
        }
        
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
    
    method add_regex_region($from_pos, $to_pos) {
        @!regex_regions.push(item $from_pos..$to_pos);
    }
    
    method add_routine_region($from_pos, $to_pos, $type, $name) {
        %!routine_regions{item $from_pos..$to_pos} = RoutineInfo.new(:$type, :$name);
    }
    
    method routine_containing($from_pos, $to_pos) {
        my @best = %!routine_regions.grep(
                { ($from_pos..$to_pos) ~~ $^r.key }
            ).sort(
                { .key.max - .key.min }
            );
        @best ?? @best[0].value.name !! ''
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
    
    sub throw_lines(@lines) {
        @lines.map: {
            colored('| ' ~ $_.subst("\r", ""), 'yellow')
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
            my $safe_start_pos = [min] $start_line_pos, .chars;
            my $safe_end_pos   = [min] $end_line_pos, .chars;
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
    
    method regex_match_status($from, $to, $ctx) {
        if ($from..$to) ~~ any(@!regex_regions) {
            my $cur = try eval_in_ctx($ctx, q[DYNAMIC::<$¢>]);
            if $cur ~~ Cursor {
                my $pos = $cur.pos;
                my $str = $cur.target;
                my $bfrom  = [max] 0, $pos - 77;
                my $blen   = [min] 77, $pos;
                my $before = $str.substr($bfrom, $blen).subst(/\n/, '\n', :g).subst(/\t/, '\t', :g);
                my $after  = $str.substr($pos,     144).subst(/\n/, '\n', :g).subst(/\t/, '\t', :g);
                if $before.chars + $after.chars > 77 {
                    if $after.chars > 43 {
                        $after = $after.substr(0, 40) ~ '...';
                    }
                    if $before.chars > (74  - $after.chars) {
                        $before = "..." ~ $before.substr(* - (74 - $after.chars));
                    }
                }
                return normal_lines(
                    [
                        colored('Regex Match Position', 'blue'),
                        colored($before, 'green') ~ $after
                    ],
                    'blue')
            }
        }
        ()
    }
    
    method summary_around($from, $to, $ctx) {
        my ($from_line, $from_pos) = self.line_of($from, 0, 0);
        my ($to_line, $to_pos)     = self.line_of($to, $from_line, $from_pos);
        my $ctx_start = $from_line - 2;
        $ctx_start = 0 if $ctx_start < 0;
        my $ctx_end = $to_line + 2;
        $ctx_end = +@!lines - 1 if $ctx_end >= @!lines;
        return join "\n",
            colored("+ $!filename ($ctx_start.succ() - $ctx_end.succ())", 'blue'),
            normal_lines(@!lines[$ctx_start..^$from_line], 'blue'),
            highlighted_lines(@!lines[$from_line..$to_line], $from_pos, $to_pos),
            normal_lines(@!lines[$to_line^..$ctx_end], 'blue'),
            self.regex_match_status($from, $to, $ctx);
    }
    
    method throw_summary($e, $line) {
        my $ctx_start = $line - 2;
        $ctx_start = 0 if $ctx_start < 0;
        my $ctx_end = $line + 2;
        $ctx_end = +@!lines - 1 if $ctx_end >= @!lines;
        return join "\n",
            colored("+ Exception Thrown", 'yellow'),
            normal_lines(lines($e.message), 'yellow'),
            colored("+ $!filename ($ctx_start.succ() - $ctx_end.succ())", 'yellow'),
            normal_lines(@!lines[$ctx_start..^$line], 'yellow'),
            throw_lines([@!lines[$line]]),
            normal_lines(@!lines[$line^..$ctx_end], 'yellow');
    }
    
    method exception_summary($e, $line) {
        my $ctx_start = $line - 2;
        $ctx_start = 0 if $ctx_start < 0;
        my $ctx_end = $line + 2;
        $ctx_end = +@!lines - 1 if $ctx_end >= @!lines;
        return join "\n",
            colored("+ Uncaught Exception", 'red'),
            normal_lines($e.message.lines, 'red'),
            colored("+ $!filename ($ctx_start.succ() - $ctx_end.succ())", 'red'),
            normal_lines(@!lines[$ctx_start..^$line], 'red'),
            error_lines([@!lines[$line]]),
            normal_lines(@!lines[$line^..$ctx_end], 'red');
    }
}

# File and line position.
my regex flpos {
    [$<file>=[<-[:]>+] ':']? $<line>=[\d+]
}

# Holds the current state of the debugger.
my class DebugState {
    my enum RunMode <Step StepOver StepOut RunToThrowOrBreakpoint RunToUnhandledOrBreakpoint>;
    my RunMode $run_mode  = Step;
    my Bool    $dying     = False;
    my Bool    $in_prompt = False;
    my $cur_ex;
    my %stepping_out_of;
    my %stepping_over_in;
    my %breakpoints;
    my %tracepoints;
    my @tp_log;
    
    my class TracePointLogEntry {
        has $.tp;
        has $.result;
        has $.fail;
    }
    
    my class TracePoint {
        has $.file;
        has $.line;
        has $.expr;
        
        method hit($ctx) {
            try {
                @tp_log.push(TracePointLogEntry.new(
                    tp     => self,
                    result => eval_in_ctx($ctx, $!expr).gist
                ));
                CATCH {
                    default {
                        @tp_log.push(TracePointLogEntry.new(
                            tp     => self,
                            result => .gist,
                            fail   => True
                        ));
                    }
                }
            }
        }
    }

    method set_current_exception($ex) {
        $cur_ex = $ex;
    }
    
    method enter_death_throes() {
        # Or is that the death throws?
        $dying = True;
    }
    
    method in_prompt() {
        $in_prompt
    }
    
    method normalize_filename($file) {
        if %sources{$file}:exists {
            return $file;
        }
        else {
            my $try_file = $file.subst(/'::'/, '/', :g);
            for %sources.keys -> $known {
                if $known ~~ /$try_file/ {
                    return $known;
                }
            }
        }
        return Nil;
    }
    
    method add_breakpoint($file, $line) {
        if self.normalize_filename($file) -> $norm_file {
            push %breakpoints{$norm_file}, $line - 1;
        }
        else {
            say colored("Cannot add breakpoint to unknown file '$file'", 'red');
        }
    }
    
    method remove_breakpoint($file, $line) {
        if self.normalize_filename($file) -> $norm_file {
            if %breakpoints{$norm_file} {
                my $rem_line = $line - 1;
                if any(%breakpoints{$norm_file}.list) == $rem_line {
                    %breakpoints{$norm_file} .= grep(* != $rem_line);
                    return;
                }
            }
            say colored("No breakpoint at line $line in $file", 'red');
        }
        else {
            say colored("Cannot remove breakpoint from unknown file '$file'", 'red');
        }
    }
    
    method is_breakpoint_at($filename, $from, $to) {
        if %breakpoints{$filename} -> @bp_lines {
            my ($from_line, $) = %sources{$filename}.line_of($from, -1, -1);
            my ($to_line, $)   = %sources{$filename}.line_of($to, -1, -1);
            return any(@bp_lines) ~~ ($from_line..$to_line);
        }
    }
    
    method add_tracepoint($file, $line, $expr) {
        if self.normalize_filename($file) -> $norm_file {
            push %tracepoints{$norm_file}, TracePoint.new(:file($norm_file), :$line, :$expr);
        }
        else {
            say colored("Cannot add tracepoint to unknown file '$file'", 'red');
        }
    }
    
    method log_tracepoints($ctx, $filename, $from, $to) {
        if %tracepoints{$filename} -> @tps {
            my ($from_line, $) = %sources{$filename}.line_of($from, -1, -1);
            my ($to_line, $)   = %sources{$filename}.line_of($to, -1, -1);
            @tps.grep({ $^tp.line - 1 ~~ ($from_line..$to_line) })>>.hit($ctx);
        }
    }
    
    method render_one_tracepoint($tp) {
        say colored(">>> ", 'blue') ~ $tp.expr;
        for @tp_log.grep(*.tp === $tp) {
            say .fail
                ?? colored("* $_.result()", 'red')
                !! colored("* ", 'blue') ~ $_.result();
        }
    }
    
    method render_all_tracepoints() {
        my $last_tp;
        for @tp_log -> (:$tp, :$result, :$fail) {
            unless $tp === $last_tp {
                say colored(">>> ", 'blue') ~ "$tp.file():$tp.line()";
            }
            say $fail
                ?? colored("* $result", 'red')
                !! colored("* ", 'blue') ~ $result;
            $last_tp = $tp;
        }
    }
    
    method should_break_at($filename, $from, $to) {
        given $run_mode {
            when Step {
                True
            }
            when StepOver {
                ENTER $in_prompt = True;
                LEAVE $in_prompt = False;
                my $depth = +lines(Backtrace.new().full) - 1;
                if $filename eq %stepping_over_in<file> &&
                        %sources{$filename}.routine_containing($from, $to) eq %stepping_over_in<routine> &&
                        $depth == %stepping_over_in<depth>
                        || $depth < %stepping_over_in<depth> {
                    $run_mode = Step;
                    %stepping_over_in = ();
                    True
                }
                else {
                    False
                }
            }
            when StepOut {
                if ($filename ne %stepping_out_of<file> || 
                         %sources{$filename}.routine_containing($from, $to) ne %stepping_out_of<routine>)
                         && (+lines(Backtrace.new().full) - 1) < %stepping_out_of<depth> {
                    $run_mode = Step;
                    %stepping_out_of = ();
                    True
                }
                else {
                    False
                }
            }
            default {
                self.is_breakpoint_at($filename, $from, $to);
            }
        }
    }
    
    method should_break_on_throw() {
        $run_mode != RunToUnhandledOrBreakpoint
    }
    
    method prompt_color() {
        $dying  ?? 'red'    !!
        $cur_ex ?? 'yellow' !!
                   'blue'
    }
    
    method complain_about_being_dying() {
        say colored(
            'Cannot continue execution after an unhandled exception',
            'red');
    }

    method issue_prompt($ctx, $cur_file, $from = 0, $to = 0) {
        ENTER $in_prompt = True;
        LEAVE $in_prompt = False;
        loop {
            given prompt(colored('> ', self.prompt_color())) {
                when !.defined { # eof
                    return
                }
                when '' {
                    $run_mode = Step;
                    $dying
                        ?? self.complain_about_being_dying()
                        !! return
                }
                when /^ < p print s say > \s+ (.+)/ {
                    say eval_in_ctx($ctx, ~$0);
                    CATCH {
                        default {
                            say colored($_.message, 'red');
                        }
                    }
                }
                when /^ < e eval > \s+ (.+)/ {
                    eval_in_ctx($ctx, ~$0);
                    CATCH {
                        default {
                            say colored($_.message, 'red');
                        }
                    }
                }
                when /^ ('$' [ \d+ | '<' \w+ '>' ]) \s* $/ {
                    say ~eval_in_ctx($ctx, ~$0);
                    CATCH {
                        default {
                            say colored($_.message, 'red');
                        }
                    }
                }
                when /^ (< $ @ % > .+ | 'self' .*)/ {
                    say eval_in_ctx($ctx, ~$0).perl;
                    CATCH {
                        default {
                            say colored($_.message, 'red');
                        }
                    }
                }
                when 'r' {
                    if $dying {
                        self.complain_about_being_dying();
                    }
                    else {
                        $run_mode = RunToUnhandledOrBreakpoint;
                        return;
                    }
                }
                when 'rt' {
                    if $dying {
                        self.complain_about_being_dying();
                    }
                    else {
                        $run_mode = RunToThrowOrBreakpoint;
                        return;
                    }
                }
                when 's' {
                    if $dying {
                        self.complain_about_being_dying();
                    }
                    else {
                        my $cur_routine = %sources{$cur_file}.routine_containing($from, $to);
                        $run_mode = StepOver;
                        %stepping_over_in =
                            file    => $cur_file,
                            routine => $cur_routine,
                            depth   => +lines(Backtrace.new().full) - 4;
                        return;
                    }
                }
                when 'so' {
                    if $dying {
                        self.complain_about_being_dying();
                    }
                    else {
                        if %sources{$cur_file}.routine_containing($from, $to) -> $cur_routine {
                            $run_mode = StepOut;
                            %stepping_out_of =
                                file    => $cur_file,
                                routine => $cur_routine,
                                depth   => +lines(Backtrace.new().full) - 4;
                            return;
                        }
                        else {
                            say colored("Not currently in a routine; cannot step out", 'red');
                        }
                    }
                }
                when 'bt' | 'st' {
                    say join "\n", lines(Backtrace.new().nice)[4..*];
                }
                when 'ex' {
                    if $cur_ex {
                        say $cur_ex.perl;
                    }
                    else {
                        say colored('No current exception', 'red');
                    }
                }
                when /^ 'bp' <.ws> 'add' <.ws> <p=&flpos> $/ {
                    self.add_breakpoint($<p><file> ?? ~$<p><file> !! $cur_file, +$<p><line>);
                }
                when /^ 'bp' <.ws> 'list' $/ {
                    for %breakpoints.kv -> $file, @lines {
                        if @lines {
                            say "$file:";
                            say "    " ~ @lines.map(*+1).join(",");
                        }
                    }
                }
                when /^ 'bp' <.ws> 'rm' <.ws> <p=&flpos> $/ {
                    self.remove_breakpoint($<p><file> ?? ~$<p><file> !! $cur_file, +$<p><line>);
                }
                when /^ 'bp' <.ws> 'rm' <.ws> 'all' $/ {
                    %breakpoints = ();
                }
                when /^ 'tp' <.ws> 'add' <.ws> <p=&flpos> <.ws> (.+) $/ {
                    self.add_tracepoint($<p><file> ?? ~$<p><file> !! $cur_file, +$<p><line>, ~$0);
                }
                when /^ 'tp' <.ws> 'list' $/ {
                    for %tracepoints.kv -> $file, @tps {
                        if @tps {
                            say "$file:";
                            for @tps -> $tp {
                                say "    $tp.line(): $tp.expr()";
                            }
                        }
                    }
                }
                when /^ 'tp' <.ws> 'show' [ <.ws> <p=&flpos> ]? $/ {
                    if $<p> {
                        my $file = $<p><file> ?? ~$<p><file> !! $cur_file;
                        if %tracepoints{self.normalize_filename($file)} -> @tps {
                            if @tps.first(*.line == +$<p><line>) -> $tp {
                                self.render_one_tracepoint($tp);
                            }
                            else {
                                say colored('No tracepoint at this line', 'red');
                            }
                        }
                        else {
                            say colored('No tracepoint in this file', 'red');
                        }
                    }
                    else {
                        self.render_all_tracepoints();
                    }
                }
                when '?' | 'h' | 'help' {
                    say self.usage()
                }
                when 'q' | 'quit' {
                    my \ENDS := nqp::getcurhllsym('@END_PHASERS');
                    if nqp::elems(ENDS) {
                        my $end_prompt = colored('- ', self.prompt_color()) ~
                            'Run END blocks (y/N)? ';
                        if prompt($end_prompt) ~~ /:i ^ y/ {
                            # Make sure we return to a sane state first.
                            $run_mode  = Step;
                            $dying     = False;
                            $in_prompt = False;
                            $cur_ex    = Nil;
                        }
                        else {
                            # Remove end blocks so we don't run them on exit.
                            nqp::bindcurhllsym('@END_PHASERS', nqp::list());
                        }
                    }
                    exit(0);
                }
                default {
                    say "Sorry, I don't understand; for help type h"
                }
            }
        }
        
        # Clear current exception on leaving here, since going on with
        # execution from an exception state leaves us in a non-exception
        # state.
        LEAVE $cur_ex = Nil;
    }
    
    method usage() {
        join "\n",
            ('<enter>                single step, stepping into any calls' unless $dying),
            ('s                      step to next statement, stepping over any calls' unless $dying),
            ('so                     step out of the current routine' unless $dying),
            ('r                      run until the next breakpoint or unhandled exception' unless $dying),
            ('rt                     run until the next breakpoint or an exception is thrown' unless $dying),
            's[ay], p[rint]         evaluate and display an expression in the current scope',
            'e[val]                 evaluate an expression in the current scope',
            '$s, @a, %h, self       show .perl of the a variable in scope (indexing allowed)',
            'bt, st                 show the backtrace from the current location',
            ('ex                     show .perl of the current exception' if $cur_ex),
            'bp add file:line       adds a breakpoint at file/line (file optional)',
            'bp list                lists all active breakpoints',
            'bp rm file:line        removes the breakpoint at file/line (file optional)',
            'bp rm all              removes all breakpoints',
            'tp add file:line expr  logs the value of expr each time file:line is hit',
            'tp list                shows a list of all set tracepoints',
            'tp show                shows the data collected by all tracepoints',
            'tp show file:line      shows the data collected by the specified tracepoint',
            'q[uit]                 exit the debugger'
            ;
    }
    
    method reset() {
        $run_mode  = Step;
        $dying     = False;
        $in_prompt = False;
        $cur_ex = Mu;
        %stepping_out_of = ();
        %stepping_over_in = ();
        %breakpoints = ();
        %tracepoints = ();
        @tp_log = ();
    }
}

# Install various hooks.
$*DEBUG_HOOKS.set_hook('new_file', -> $filename, $source {
    unless $filename eq '<unknown>' {
        say colored('>>> LOADING ', 'magenta') ~ $filename;
    }
    %sources{$filename} = SourceFile.new(:$filename, :$source);
});
$*DEBUG_HOOKS.set_hook('routine_region', -> $filename, $from_pos, $to_pos, $type, $name {
    %sources{$filename}.add_routine_region($from_pos, $to_pos, $type, $name);
});
$*DEBUG_HOOKS.set_hook('statement_simple', -> $filename, $ctx, $from, $to {
    DebugState.log_tracepoints($ctx, $filename, $from, $to);
    if DebugState.should_break_at($filename, $from, $to) {
        say %sources{$filename}.summary_around($from, $to, $ctx);
        DebugState.issue_prompt($ctx, $filename, $from, $to);
    }
});
$*DEBUG_HOOKS.set_hook('statement_cond', -> $filename, $ctx, $type, $from, $to {
    DebugState.log_tracepoints($ctx, $filename, $from, $to);
    if DebugState.should_break_at($filename, $from, $to) {
        say %sources{$filename}.summary_around($from, $to, $ctx);
        DebugState.issue_prompt($ctx, $filename, $from, $to);
    }
});
$*DEBUG_HOOKS.set_hook('regex_region', -> $filename, $from_pos, $to_pos {
    %sources{$filename}.add_regex_region($from_pos, $to_pos);
});
$*DEBUG_HOOKS.set_hook('regex_atom', -> $filename, $ctx, $from, $to {
    DebugState.log_tracepoints($ctx, $filename, $from, $to);
    if DebugState.should_break_at($filename, $from, $to) {
        say %sources{$filename}.summary_around($from, $to, $ctx);
        DebugState.issue_prompt($ctx, $filename, $from, $to);
    }
});
$*DEBUG_HOOKS.set_hook('reset', -> {
    DebugState.reset();
});
$*DEBUG_HOOKS.set_hook('new_breakpoint', -> $filename, $pos {
    DebugState.add_breakpoint($filename,
        %sources{$filename}.line_of($pos, -1, -1)[0] + 2);
});

# Allow interception of throwing an exception.
my $IN_UNHANDLED = 0;
my $IN_THROWN = 0;
my $CUR_EX;
&EXCEPTION.wrap(-> | {
    my Mu $vm_ex := nqp::atpos(nqp::p6argvmarray(), 0);
    my $e = callsame;
    unless $IN_UNHANDLED || $IN_THROWN || DebugState.in_prompt {
        if DebugState.should_break_on_throw() {
            $IN_THROWN = 1;
            $CUR_EX = $e;
            nqp::p6invokehandler(&thrown, $vm_ex);
            $IN_THROWN = 0;
        }
    }
    $e
});
sub thrown(|) {
    my $e = $CUR_EX;
    my $bt = $e.backtrace();
    my $ctx = CALLER;
    my ($file, $line);
    my $fail = False;
    for @$bt {
        if .code && .code.name eq '&fail' {
            $fail = True;
            last;
        }
        if %sources{.file}:exists {
            $file = .file;
            $line = .line;
            last;
        }
        $ctx = $ctx.WHO.<CALLER>;
    }
    if !$fail && $file {
        DebugState.set_current_exception($e);
        say %sources{$file}.throw_summary($e, $line - 1);
        DebugState.issue_prompt($ctx.WHO, $file);
    }
}

# Override handler for uncaught exceptions.
my Mu $p6comp := nqp::getcomp('perl6');
$p6comp.HOW.find_method($p6comp, 'handle-exception').wrap(-> | {
    my Mu $vm_ex := nqp::atpos(nqp::p6argvmarray(), 1);
    nqp::p6invokehandler(&unhandled, $vm_ex);
});
sub unhandled(|) {
    $IN_UNHANDLED = 1;
    my Mu $vm_ex := nqp::atpos(nqp::p6argvmarray(), 0);
    my $e = EXCEPTION($vm_ex);
    my $bt = $e.backtrace();
    my $ctx = CALLER;
    my ($file, $line);
    for @$bt {
        if %sources{.file}:exists {
            $file = .file;
            $line = .line;
            last;
        }
        $ctx = $ctx.WHO.<CALLER>;
    }
    if $file {
        DebugState.enter_death_throes();
        DebugState.set_current_exception($e);
        say %sources{$file}.exception_summary($e, $line - 1);
        DebugState.issue_prompt($ctx.WHO, $file);
    }
    else {
        say $e;
        exit(0);
    }
}
