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
    
    make

Then to test it:

    make test

If all tests return normal, then you can compile some of the example's by 
running:

    make examples

Since ErLam compiles to Erlang's EScript by default, if on linux you can simply
run an example (e.g. fibonacci) like so:

    ./examples/fib.ex


### Examples

What better way to explain a language than to provide some examples. The 
following are a few popular examples to demo ErLam. (Note the canonical hello
world is not possible as this language does not have strings.) For more examples
including the ones below, check out the `examples` directory.

#### Example program: Fibonacci

Not parallelized, but a simple Fibonacci function with continuation.

    fun n. (rep n (fun f,a,b.(f b (add a b))) left 0 1)

Where the `rep` function takes an integer and returns the Church Numeral 
function for it. In otherwords, replicate the function call `n` times.

To then parallelize, it's a bit more difficult as we need to synchronize using 
channels. The following function spawns two new sub-processes for both numbers
and then waits for their return:

    (fun n.( fun x.(x x)
             (fun f,m. 
                  if (lt m 1) 
                     m
                     (merge fun _.(f f (sub m 1))
                            fun _.(f f (sub m 2))
                            add)
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
we hit the merge point.
       

#### Example program: Ping-Pong Servers

Another common example is a ping-pong server, where a client and server pass
a message back and forth. Note that ErLam uses swap channels so we force mutal
exclusion:

    fun n.(
        let c = newchan in
        let server = (fun _.(omega fun s. 
                                    let r = (swap c nil) in
                                     (if (eq r nil) 
                                         (print ~1) 
                                         (ignore (print r) (s nil))))) in
        let worker = fun t.(omega (fun w,x.
                            (if (leq x 0)
                                (swap c nil) 
                                (ignore (swap c x) (w w (dec x))))) t)
        in (ignore (spawn server) (worker n)))
    100

This will run a worker which will communicate with the spawned server 100 times 
before closing the channel. The example can be modified to wait a random number 
of seconds before sending back a message (as in the `examples` directory).

Note that the `omega` function is the shorthand for the application function 
used in the Fibonacci example: `fun x.(x x)`.

