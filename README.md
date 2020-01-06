# Copycat: a concatenative language (using S-expressions)

This implements a concatenative ("Forth-like") language. It uses
S-expressions for basic syntax, and deviates from
[Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)) by
using lists to quote subprograms, and maybe a few other changes. It
might be closest
[to](http://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language)
[Joy](https://en.wikipedia.org/wiki/Joy_(programming_language)), or
[tcK (tiny concatenative K)](http://archive.vector.org.uk/art10000360). See
also [Cat](http://www.cat-language.com/).

## Installation and usage

### Dependencies

This requires the Gambit-C Scheme system with some patches as
available [here](https://github.com/pflanze/gambc.git); on a Debian
(perhaps Ubuntu) system it can easily be installed via
[chjize](https://github.com/pflanze/chjize) by following the
instructions there then running `make gambit`.

### Build

    git clone https://github.com/pflanze/copycat
    cd copycat
    git submodule init
    git submodule update

### Build

    cd copycat
    gsc -:tE,dar,t8,f8,-8
    > (run-tests ".")
    > ,q

### Run read-eval-print-loop

    cd copycat
    gsc -:tE,dar,t8,f8,-8
    > (cc-repl)

