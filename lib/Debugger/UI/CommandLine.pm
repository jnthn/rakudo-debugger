module Debug::UI::CommandLine;

# The source code of the files we've encountred while debugging.
my %sources;

$*DEBUG_HOOKS.set_hook('new_file', -> $filename, $source {
    say ">>> LOADING $filename";
    %sources{$filename} = $source;
});

$*DEBUG_HOOKS.set_hook('statement', -> $text {
    say $text;
    prompt("> ");
});
