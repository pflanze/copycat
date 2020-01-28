# Introduction to Copycat

Copycat is a
[concatenative programming language](https://en.wikipedia.org/wiki/Concatenative_programming_language). It
uses a single stack from which inputs for calculations are taken and
onto which results from calculations are pushed. In the following
examples we're using the Copycat read-eval-print loop ("REPL"), which
can be started via `$PATH_TO/copycat/bin/copycat-repl`. It will show:
    
    ()
    100000 @ 

The `()` is representing the stack (as a list), because we have just
started the REPL it is the empty list.  The `100000` is the current
amount of "fuel" left for the "Copycat machine" to run--if it reaches
zero, the machine will throw an exception which (currently) aborts
evaluation of the program (see the "Fuel" section for more
details). The `@` is the command prompt, asking you to enter a single
line of input in the Copycat language. There is line editing and
history support (cursor up and down, ctl-r etc.) provided by `rlwrap`
(see `man rlwrap`). After hitting the return key, the input is
evaluated; any values resulting from the evaluation are left on the
stack and shown before showing another `@` input prompt. To interrupt
long-running calculations, press Ctrl-C (TODO: this needs improvement,
as it currently doesn't interrupt long-running system calls, and it
doesn't abort a partially entered line in the line editor.)  To exit
the repl, write `quit` (or `0 exit`), or hit Ctrl-D currently.

Copycat aims to provide good facilities for functional
programming--mutation should rarely be needed.


## Syntax

Copycat is built using
[S-expressions](https://en.wikipedia.org/wiki/S-expression), which
means that the first level of syntax is the one defined by the
S-expression parser of the
[Scheme programming language](https://en.wikipedia.org/wiki/Scheme_(programming_language))
(strings and files are first passed to the S-expression parser, and
only afterwards processed further).

Let's quickly introduce some basics and then go into the syntax in
more detail.

Example: type `5` then hit return:

    100000 @ 5
    (5)

The number 5 is now on the stack, as shown with `(5)`. Enter another
number (the fuel number was decremented by one, as there was one
action that was run):

    99999 @ 7
    (7 5)

The stack now holds both `5` and `7`. The values on the top of the
stack (entered last) are shown to the left. Enter `+` and return:

    99998 @ +
    (12)

The program (action) associated with `+` has taken 2 values off the
stack, added them up, and placed the result back onto the stack.

    99997 @ number?
    (#t)

The `number?` action has taken 1 value off the stack, checked whether
it is a number, and because that is the case, put the boolean value
representing true, `#t`, onto the stack.

More than one expression can be entered at the same time:

    99996 @ 10 14 /
    (5/7 #t)

This pushed another `10` and `14` onto the stack then ran a division
(which, since the arguments of the division are exact numbers, returns
a simplified fraction). `#t` is still on the stack from before. A
program in Copycat consists simply of a list of items to be evaluated.

This document will omit the fuel number from now on as it's
unimportant for the explanations easily gets out of sync while editing
this document.

### Literals

Literals are syntactical items which represent values directly
(i.e. they do not lead to a computation). The values they represent
are simply pushed onto the data stack.

#### Booleans

There are 2 boolean values, `#f` to represent false, and `#t` to
represent true.

#### Numbers

This implementation of Copycat supports the full
[numerical tower](https://en.wikipedia.org/wiki/Numerical_tower)
provided by Scheme. This includes `integer` (which includes `fixnum`,
small integers that fit as immediate values, and are enough to denote
indices into data structures held in memory, and bignums), `rational`
and `real` (both `exact`, consisting of exact integers and ratios of
those, and floating point numbers (`inexact`)), and `complex` (again
both `exact` and `inexact`). 

Examples (`c` clears the stack):

    @ c -2 exact?
    (#t)
    @ c -2 sqrt
    (+1.4142135623730951i)
    @ 3 +
    (3+1.4142135623730951i)
    @ dup exact? swap complex?
    (#t #f)

#### Characters

    @ c #\A char?
    (#t)
    @ c #\A println
    A
    ()
    @ #\A char.integer
    (65)

#### Strings

Series of characters. They are as defined by the Scheme standard.

    @ c "Allô" "Motörhead!"
    ("Motörhead!" "Allô")
    @ println
    Motörhead!
    ("Allô")
    @ dup string? swap string.list
    ((#\A #\l #\l #\ô) #t)
    @ (char.integer) list-map
    ((65 108 108 244) #t)
    @ (inc integer.char) list-map
    ((#\B #\m #\m #\õ) #t)
    @ char-list.string
    ("Bmmõ" #t)

#### Lists

Series of other items. List literals start with `(` and end with
`)`. There are no commas between elements in a list.

Lists are implemented as chained pairs (singly-linked lists), each
pair holds a value, and the remainder of the list. The empty list is
represented by the null value (meaning, a list is either a pair or
null object). Lists are immutable, they can only be changed by adding
new pairs at the front, or dropping pairs--or combinations thereof
(lists are stacks, just like the data stack; a list can be set as the
data stack via `set-stack`, and the current stack can be retrieved as
a whole (put on top of itself) via `get-stack`). Due to this,
extending a list at the front is fast (using `cons` or `consn`), as is
removing items from the front (using `rest` or `drop`), but accessing
or modifying items far into the list is slow. You may want to use
vectors instead if fast access to random elements is important.

Here's an example on how to construct a list item by item, starting
from the empty list:

    @ c () 123 cons "foo" cons
    (("foo" 123))

#### Vectors

Series of other items, like lists. Vector literals start with
`[` and end with `]` (this is an extension to the standard Scheme
S-expression syntax). As with list syntax, there are no commas between
vector items.

Unlike lists, vectors are implemented as single blocks of memory; for
this reason, unlike for lists, reading elements at random positions
(using `vector.ref`) is fast. Vectors can be mutated in place using
`vector.set!` (which is fast). There's also a `vector.set` operation
which does not mutate its argument, but (with the current
implementation) it's inefficient as it copies the whole vector.

### Symbols (words)

Strings of non-whitespace characters without double quotes around them
are (as long as they are not parsed as something else like numbers,
lists, single quotes etc. instead) symbols (or words, as Forth prefers
to call them). When the evaluator encounters them, it retrieves the
program associated with them (an exception is thrown if no program is
associated with them) and evaluates it before continuing with the next
item in the current program.

Note that symbols can contain punctuation like `.`, `?`, `!`, `-`. See
"naming conventions" for how these are usually used.

Symbols can also contain whitespace characters and other special
characters if they are enclosed with vertical bar character:

    @ c '|foo \|bar's| symbol.string
    ("foo |bar's")
    @ string.length
    (10)

### Additional syntax

* `;` is used to introduce a line comment--the rest of the line is
    being ignored. `#;` is used to comment out the following
    s-expression. `#| .. |#` can be used to comment out a region (but
    if you can get away with `#;` that's nicer, especially with editor
    support).

* `'` (a single single-quote character): prevents evaluation of the
    item right to it, i.e. turns it into a literal value. For items
    which are self-quoting (literals) anyway, it doesn't have any
    effect, but it's necessary for symbols (words) if what you want is
    not their evaluation, but them being put onto the stack:
  
        @ c 'help ref
        ([(ccguestproc)
                   "print help on the given word"
                   [(cc-type/results) #f ([symbol? word]) ()]
                   (help-string println)])

    `'foo` is equivalent to `(quote foo)`, and in fact the single quote
    is expanded into the latter form by the parser before it reaches the
    interpreter.

        @ c 'foo symbol?
        (#t)
        @ c ''foo symbol?
        (#f)
        @ c ''foo list?
        (#t)
        @ c ''foo 0 list-ref
        (quote)
        @ symbol?
        (#t)
        @ c ''foo 1 list-ref
        (foo)

    Quoting via `'` is also necessary to prevent lists from being
    understood as programs and really ensure they are being treated as
    list literals.

* colon, `: name (prog...)`: associates `name` with
    `(prog...)`. Equivalent to `(prog...) 'name set!`, but follows
    Forth's syntax and is probably nicer to read.

* colon as the first item in a sub-list, `(: name type...docstring
    (prog...))`: same as `: name (prog...)` but allows to additionally
    declare input and (optionally) output types, and an optional
    docstring.

    A type declaration looks like:
  
        somearg [boolean? otherarg] -> string?
  
    This declares that the procedure being defined always takes (at
    least) 2 items from the stack, the first (put on the stack first,
    but is now second from the top of the stack) being `somearg` with
    no type constraint, and the second `otherarg` which has the
    constraint that the procedure `boolean?` must return true if
    passed the value (type items which are vectors define both the
    predicate restricting the type, and the variable name). If no
    arrow is given, that means that the return types are
    unspecified. If the arrow is given but nothing to the right of it
    that means no return value. If the symbol `!` is given right of
    the arrow, that means that the procedure never returns (it might
    exit via an exception, though).

    Note that currently the variable names and type constraints (when
    used via `:` from Copycat--things are different when using the
    `cc-def` etc. forms from Scheme) are purely cosmetic (for
    documentation, and shown by `help`). They are neither assigned to
    those symbols, nor are the type checks being done. This may both
    change in the future.

    Not all predicate functions currently being used are available in
    Copycat (they should be available in the underlying Scheme,
    though). (TODO)

    Currently it's also accepted to give variables defined in the inputs
    instead of predicates as the results, like:
  
        a b -> b a

    (Where this is currently being used, it actually does refer to the
    identical values passed as arguments; it might be widened to mean
    the same types, but that would need a way to parametrize the types.)
    It's not clear yet how this will be handled in the
    future. (Suggestions welcome.)


## Definition context

Currently there's only one single global context for symbols. This may
change in the future (either or both to allow for modules and imports,
and for lexical variables (closures)).

The list of defined symbols can be retrieved via `dir`.


## Development aids

* `'name help`: show information about the given name, including
    docstring and type declaration given when defining it.

* `(prog...) time`: show how long evaluation of the given program
    takes (including how much (temporary) memory it allocates)
  
* `P`: print the stack at the given point of the program (without
    consuming it), prefixed by a line that indicates location (and makes
    Emacs highlight the printed location)

* `PS`: same as `P` but turns the items into Scheme constructor syntax
    (via `show`)

* `"msg" PM`: same as `P` but instead of prefixing with the location,
    prints "msg"

* `D`: enter a sub-repl in which you can interact with the stack; hit
    ctl-d on an empty input line to continue evaluation of the
    surrounding program. The sub-repl shows the level of nesting
    between the fuel number and the prompt. Example:

        100000 @ 1 2 3 D 4 5 *
        (3 2 1)
        99996 1 @ 1 +
        (4 2 1)
        99994 1 @ <Ctl-D>
        (20 4 2 1)
        99991 @ 


## Errors / exceptions

If an error happens during evaluation of a program (like a failing
dynamic type check, running out of fuel, etc.), an object with a
sub-type of `copycat-error` is allocated to hold information about the
error, and then instead of continuing evaluation of the current
program, evaluation is stopped and instead passes back the error
(implicitly as part of the evaluator API, not via the stack). Normally
and currently (there are plans to offer calling into a debugger
instead), when running the program from a REPL, the latter prints a
message stating the error, and issues a new prompt.

Copycat programs can evaluate a sub-program without being interrupted,
though, via `try`:

    1000 @ c (12 2 /) try
    ([(Ok) (6)])
    994 @ c (12 0 /) try
    ([(Error) [(copycat-division-by-zero) / 12]])
    991 @ 

Failing programs currently do not cost fuel. That's why the second
case above took less fuel, only the outer part of the program cost
fuel.

To use the result of `try`, use `if-Ok`, which will run the
corresponding branch with the value wrapped in Ok or Error on the
stack:

    @ c (12 2 /) try ("succeeded") ("failed") if-Ok
    ("succeeded" (6))
    @ c (12 0 /) try ("succeeded") ("failed") if-Ok
    ("failed" [(copycat-division-by-zero) / 12])

To use the stack that was wrapped in `Ok` in the success branch, use
`set-stack`:

    @ c (12 2 /) try (set-stack "succeeded") ("failed") if-Ok
    ("succeeded" 6)


## Tail-call optimization

The Copycat interpreter performs
[tail-call optimization](https://en.wikipedia.org/wiki/Tail_call): if
the last item in a program (a list of actions) is a procedure call
(i.e. the call is from the tail end of the program), the interpreter
will continue with evaluation of that new procedure as if the
currently executing one already returned--or in other words, it does
not remember to return to the current program. Why should it, there's
nothing left to be done in the current program after running the new
one. This means that for example loops can be written by defining a
word and calling it recursively from the tail position; this does not
lead to increasing call stack memory usage on each iteration.


## Naming conventions

* Procedures which return a single boolean are named with a trailing
    question mark.

* Procedures which mutate data structures (regardless of whether
    in-memory or out of process) are named with a trailing exclamation
    mark. Those which take the data structure they mutate as an
    argument, and have a sibling procedure which does not mutate the
    data structure (of the same or another, equivalent type) but instead
    return a new version of the data structure, also return the mutated
    data structure (even though not necessary since keeping a
    `dup`licate of the argument before calling the mutator would yield
    the same result)--this is so that code can be written first to use
    the functional procedure, and then changed to do the (faster)
    mutation by just adding the exclamation mark (where that is safe to
    do without changing the meaning of the program).

* Procedures which work for various types via type dispatch (methods)
    are named with a leading dot.


## Files

Programs can be loaded from files via `"the/path.scm" load`. It is
currently recommended to use the `.scm` suffix for files containing
Copycat code, to easily get the correct editor support (for Scheme,
and thus for the S-expressions used for writing Copycat). (Note also
in particular Paredit (or Parinfer) on Emacs and some other editors.)

Have a look at [examples/](../examples/).

Other file based commands are `current-directory` or `pwd`,
`set-current-directory` or `cd`, `directory-items`, and `ls`.


## Fuel

The interpreter runs on a limited (but possibly very high) amount of
fuel. Each program step costs one unit of fuel for its evaluation. When
fuel reaches zero, evaluation stops (aborts) with an exception. This
exception can be trapped with `try`. As mentioned in the section on
exceptions, currently failing programs (regardless of whether they
failed because of running out of fuel or for another reason) do "give
back" their fuel, i.e. they cost nothing. This means that there will
be fuel left after `try` to handle the error.

Fuel can be added via `add-fuel` (relative) or `set-fuel` (absolute).

(Possibly the copycat-repl should automatically reset the fuel to a
given amount on each interactive entry? Not done currently as it might
be educative to deal with it manually.)


## Turtle graphics

As a demo and for educational purposes, a module
([copycat-turtle.scm](../copycat-turtle.scm)) is bundled (and
currently always loaded) which defines procedures to allow to write
programs to generate
[turtle graphics](https://en.wikipedia.org/wiki/Turtle_graphics).

The `copycat-repl` automatically opens the viewer if any of the turtle
commands are being used (and updates the view whenever more commands
are being used, once after returning from the program--regardless of
whether the programm succeeded or not). (TODO: exception: `new` if the
drawing command queue is already empty, since it doesn't lead to a
change in it, does not lead the repl to issue the viewing command,
hence will not clear any existing SVG output or open the viewer. This
should probably be fixed.)

When using `copycat` without the repl, `view` has to be called to open
the viewer or update the view.

The viewer is currently hard coded to be `eog` (Eye of Gnome, needing
X Windows on Linux), although if the `DISPLAY` environment is not set,
no viewer will be started (the `logo.svg` output file is being
generated in the current working directory, and could be picked up by
another program).

Have a look at the "turtle" examples in [examples/](../examples/).

