module Debug::UI::CommandLine;

$*DEBUG_HOOKS.set_hook('statement', -> $text {
    say $text;
    prompt("> ");
});
