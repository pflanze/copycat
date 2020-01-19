# Copycat: a concatenative language (using S-expressions)

This implements a concatenative ("Forth-like") language. It uses
S-expressions for basic syntax, and deviates from
[Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)) by
using lists to quote subprograms, and maybe a few other changes. It
might be
[closest](https://hypercubed.github.io/joy/html/forth-joy.html)
[to](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language)
[Joy](https://en.wikipedia.org/wiki/Joy_(programming_language)),
[or](http://www.cat-language.com/)
[Cat](https://github.com/cdiggins/cat-language).  See also
[tcK (tiny concatenative K)](http://archive.vector.org.uk/art10000360).

## Installation and usage

### Dependencies

This requires the Gambit-C Scheme system with some patches as
available [here](https://github.com/pflanze/gambc.git); on a Debian
(perhaps Ubuntu or Raspbian) system it can easily be installed via
[chjize](https://github.com/pflanze/chjize) by following the
instructions there then running `make gambit`.

`rlwrap` is required by the `copycat-repl` script to provide better
line editing and history functionality.

### Build

    git clone https://github.com/pflanze/copycat
    cd copycat
    git submodule init
    git submodule update

### Build

This compiles any uncompiled module files (or module files which
changed since the last build), then runs the test suite:

    $PATH_TO/copycat/bin/copycat-test

### Run read-eval-print loop

This also first builds any unbuilt or modified files, then listens for
command input in the Copycat language:

    $PATH_TO/copycat/bin/copycat-repl


## Documentation

Read [doc/intro](doc/intro.md) for a description of the language.

## Future

These are my plans for what I intend to implement still:

- lexical variables
- check the type declarations dynamically
- first-class continuations
- debugging: instead of ending calculation on uncaught exceptions, go
  into a debugger
- perhaps a module system / namespaces?
- compile Scheme to Copycat?
- check types statically (as far as possible)?
- (translator from Copycat to PostScript?)

Ideas:

- use `( )` for Scheme style syntax, `[ ]` for Copycat programs? Could
  provide `(if (zero? x) [sqrt] [inc square])` that way and `show`
  output would become natural (mostly). Should provide efficient
  functional vec implementation then, though.

