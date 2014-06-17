# ErLam: Erlang Lambda Compiler

Erlam is a compiler for an experimental derivative of Lambda Calculus with 
integers, and swap channels. It's being geared for testing various scheduling 
systems. We chose the bare necessities for our language and are avoiding all 
boilerplate with minimum syntax sugar.

There are only three values: Integers, Functions and Channels. Here's the 
expression BNF with their explanations:

    Expression ::= x             # Variables
                 | n             # Integers
                 | (E)           # Expressions can be grouped
                 | E E           # Application of expressions
                 | if E E E      # If clause on natural numbers, true=!0
                 | swap C E      # Blocking Swap on channel C with value E
                 | spawn E       # Spawn process to handle expression E
                 | fun x.E       # Function Definition (single variable, or _ to ignore)
                 | newchan       # Generate unique Channel (used in swap)

This is the extent of the language. We will have only a small standard library 
of internal functions that can be used to simplify some things like math, IO,
and common functional techniques.

### Syntactic Sugar

I would like to add two forms of syntactic sugar though, `let` expressions:

    let x = E1 in E2
        := (fun x. E2) E1

And multi variable functions (which can still be curried, from left to right):
    
    fun x,y,z. E
        := fun x. (fun y. (fun z. E))

Both expressions are quickly converted to their non-sugar forms during parsing.


### Usage

To use ErLam, you will need the Erlang VM installed. This is the only 
dependency (apart from possibly git, so you can download this repository). To 
build Erlam, just run:
    
    $ make

Then to test it:

    $ make test

If all tests return normal, then you can compile some of the example's by 
running:

    $ make examples

Since ErLam compiles to Erlang's EScript by default, if on linux you can simply
run an example (e.g. fibonacci) like so:

    $ ./examples/fib.ex

To check out some Runtime information about the program, append the `-v` option
when running it. This will build an event log to the current directory. You can
then optionally run the report generator which will generate (by default) a PDF
of some descriptive charts:

    $ ./examples/pfib.ex -v
    VERBOSE: Logs will be pushed to: pfib-erlam_sched_global.log
    Res: 55
    $ ./bin/reportgen pfib-erlam_sched_global.log

The report generator uses the R language to perform analysis and chart gen.
You will need the `ggplot2`, `reshape2`, and `RColorBrewer` packages to take 
advantage of the report generator. Note adding `--png` to the `reportgen` call
will generate PNG's instead of a PDF (you can also export jpegs, bmps, and 
tiffs).

### Examples

What better way to explain a language than to provide some examples. The 
following are a few popular examples to demo ErLam. (Note the canonical hello
world is not possible as this language does not have strings.) For more examples
including the ones below, check out the `examples` directory.

#### Example program: Fibonacci

Not parallelized, but a simple Fibonacci function with continuation.

    fun n. (rep n (fun f,a,b.(f b (add a b))) left 0 1)

Where the `rep` function takes an integer and returns the Church Numeral 
function for it. In other-words, replicate the function call `n` times.

To then parallelize, it's a bit more difficult as we need to synchronize using 
channels. The following function spawns two new sub-processes for both numbers
and then waits for their return:

    fun n.( fun x.(x x)
            (fun f,m.
                 if (leq m 1)
                    m
                    (merge fun _.(f f (sub m 1))
                           fun _.(f f (sub m 2))
                           add))
            n)

Here we use the merge function, which spawns the last two functions as separate
processes and then merges their return values via the provided `add` function.
As we can't directly access an evaluated spawned process, `merge` wraps the call 
in a channel. Here's the `merge` function:

    fun a. let x = newchan 
           in (
                (fun q,p. p)
                (spawn (fun _.swap x (a nil)))
                (fun b. let y = newchan 
                        in (
                             (fun q. (fun p. p)) 
                             (spawn (fun _. swap y (b nil)))
                             (fun m. (m (swap x nil) (swap y nil)))
                            )))

Note we start the first process as soon as we get it and will spawn it off until
we hit the merge point. We also point out that a 'process' is a closure and a
unit-function which will be passed `nil` to bootstrap it's evaluation. Both the
built-in `spawn` function and the std-library's `merge` use this approach.

#### Report Generation for Fibonacci

The example parallel Fibonacci program above has been copied down into the 
`examples` directory so that the following example can be performed verbatim.
For this example we would like to generate a PDF report for our multi-core
work-stealing scheduler for a single execution of pfib.els (our above code).

    $ ./bin/els ./example/pfib.els
    Compiling: +to_escript
    Successful save of: ./examples/pfib.ex
    $ ./example/pfib.ex -v -s erlam_sched_multi_ws -- shared_queue
    VERBOSE: Logs will be pushed to: .../pfib-erlam_sched_multi_ws.log
    Res: 55

Note here we ran the compiled `pfib.ex` script in verbose mode using the 
scheduler implementation `erlam_sched_multi_ws`, with the option `schared_queue`
turned on. This told the work-stealing scheduler to allow other schedulers to 
have direct access to their neighbor's queues. We can now generate a report
using the exported log:

    $ ./bin/reportgen pfib-erlam_sched_multi_ws.log

You can see the exported charts in `./bin/charts/example`
