# A Command-line Frontend for Rakudo's Debugger

**Before you use this:** this debugger UI, and the internals it drives, have been
superseded by the new MoarVM Remote Debug support. There are both [GUI](https://commaide.com/)
and [command line](https://github.com/edumentab/p6-app-moarvm-debug) clients for it.
It's unlikely this debugger interface will be further developed, and in time the API it
draws on will likely be deprecated. This module was what I had time and resources to make
happen when I did it. The VM-backed remote debug support that was since implemented is far
better; it handles threads properly, can trace into the internals, etc.

This is a command-line front-end for the Rakudo Debugger. When you build a current
Rakudo Perl 6, you will get perl6-debug binaries that require this (or a compatible)
module to be installed.

To use the debugger, just run your script with perl6-debug or perl6-debug-m rather than perl6. It
takes the same set of options as the normal perl6 executable, such as -I and -M.

For information on available commands, type h then hit enter for the help
screen once you're in the debugger. You may also like to see:

* Blog walkthrough: https://perl6advent.wordpress.com/2012/12/05/a-perl-6-debugger/
* Video: http://electure-ms.studiumdigitale.uni-frankfurt.de/vod/clips/mako67J3Gj/flash.html
