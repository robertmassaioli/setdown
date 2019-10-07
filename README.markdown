# Setdown - Line based set manipulation

Author: [Robert Massaioli][6]  
Created in: 2015  

## Installation Steps

If you just want to install setdown then you just need to make sure that you have Haskell and Cabal
installed and then:

    cabal install setdown

This works because [setdown is on hackage][7]!

## What is setdown and how does it work?

Setdown is a command line tool for line based set operations. To use setdown you write a "setdown
definitions file" often suffixed with **.setdown**. If you are familiar with [Make][3] then you can think
of this **.setdown** file much like a Makefile. Inside that file you write a number of
definitions of the form:

    definitionName: "file-1.txt" /\ "file-2.txt"

This line says that "definitionName" is a new set definition that is a label for the intersection of
"file-1.txt" and "file-2.txt". You can write more complicated expressions that this.

### Example Setdown Projects

[Checkout the setdown-examples project][2] on Bitbucket; it will show you how setdown works.

However, to get an in-depth description of setdown and its abilities you should
read the sections below.

### Input Files

In setdown *each file is treated as a list of elements where each line
is an element*. Input files do not need to begin as sets; they can contain duplicate and unsorted
elements. Setdown will automatically sort and de-duplicate all input files, turning them into sets.

Another important point is that of relativity: specifically, if have a **.setdown** file that
references the input file "some-elements.txt" and I run the setdown executable from a directory that
is not the same directory as the **.setdown** file then where will setdown look for the
some-elements.txt file? The answer is that we always look for files relative to the **.setdown**
file. That is where you wrote your definitions so the paths are relative to that. It was designed in
this way so that you could run setdown from anywhere in the directory tree and still get the same
result. It was an important design of setdown that you always get the same result every time that you run it.
Setdown has been designed to be current working directory invariant, as opposed to many
other command line programs. Please keep this in mind.

### Set Operations and Precidence

In the setdown language there are a number of supported operators:

 - Intersection: /\
 - Union: \/
 - Difference: -

For example, they might be used in the following way:

    definition: (A - B) \/ (C /\ D)

You may be wondering what [operator precidence][1] the setdown language uses and the answer is:
there is no operator precidence at all, instead *you must clearly specify the precidence of nested
expressions with brackets*. This is very important because it will result in parsing errors
otherwise. To show you why I made this decision lets show you an example:

    -- Here is a simple expression
    def: A /\ B \/ C
    -- Now, should this be parsed as:
    defV1: (A /\ B) \/ C
    -- or as:
    defV2: A /\ (B \/ C)
    -- If you pretend that B is the empty set (E) then you can see that these expression evaluate
    -- completely differently. If we simplify them with that assumption then they become:
    defV1-bempty: E
    defV2-bempty: A /\ C

So as you can see, order of operations really matters for set operations. Because it is so critical
I decided to make the use of brackets mandatory. Sorry for the extra brackets but you will thank me
when you expressions come out exactly the way that you expect them to.

### Comments

In the setdown language you can add comments by writing a double-dash (--) and then writing the
comment till the end of the line. The following comments are valid:

    -- This is a definition for A, created because we wanted to do X
    A: "y.txt" - "z.txt"

    -- This is an example of a comment halfway through an expression
    B: (A \/ C) -- \/ D This is still a comment and \/ D never happens

You can use comments to leave messages for any people that might read your setdown definitions in
the future. It may help explain to them what you were trying to do.

### Writing your own definitions

In the setdown language you can write a definition in the following format:

     <definitionName>: <expression>

Where the definition name is the identifier that you give to that expression. An expression is the
application of set operations on identifiers or files. A practical example of what this looks like
should help cement what this means. Here is a valid setdown file:

    -- A is the intersection of the file b-1.out and the set B
    A: "b-1.out" /\ B

    -- B is the union of the file a-1.out and a-2.out
    B: "a-1.out" \/ "a-2.out"

    -- C is the difference of the file b-1.out and the set B
    C: "b-1.out" - B

Usually, when you write these definitions you put them in a file that has a suffix of **.setdown**.
You can then feed this file into the setdown executable like so:

    setdown path/to/mydefinitions.setdown

For more information on the options that you can pass to the setdown executable try running

    setdown --help

And good luck!

## Building the code

To build the code for this project just have [Haskell installed][4] and [cabal][5] and then:

    cabal sandbox init
    cabal install

And that should have the code built on your machine. Then, if you modify the code, just use cabal run to run setdown:

    cabal run -- --help
    cabal run mydefinitions.setdown

That is all that there is to it!

### With nix for local development

If you want to install setdown locally using nix for local development then do the following:

    $ nix-shell
    $ cabal sandbox init
    $ cabal install

That should install setdown in development mode locally.  

## Contributing to the setdown project

If you wish to contribute to the setdown project then please just:

 1. Raise an issue with what you intend to fix / improve.
 1. Wait for Robert Massioli to get back to you and give you the thumbs up. If you get Robert
    Massaioli's "merge approval" then that means that, if you write the code to Roberts satisfaction
    then it will be merged in.
 1. Write the code.
 1. Raise a PR and ask Robert Massaioli to review it. (Maybe iterate a bit to get it cleaned up)
 1. Get it merged in.
 1. Celebrate!

I would love to have contributions to the project and, even though it may look like a complicated
process just follow it because it is designed to make your life, and my life, easier. Cheers!

 [1]: http://en.wikipedia.org/wiki/Order_of_operations
 [2]: https://bitbucket.org/robertmassaioli/setdown-examples
 [3]: http://www.gnu.org/software/make/
 [4]: https://www.haskell.org/platform/
 [5]: https://www.haskell.org/cabal/
 [6]: https://robertmassaioli.wordpress.com/
 [7]: http://hackage.haskell.org/package/setdown
