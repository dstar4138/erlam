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
                 | fun x.E       # Function Definition (single variable, or _)
                 | newchan       # Generate unique Channel (used in swap)

This is the extent of the language. We will have only a small standard library 
of internal functions that can be used to simplify some things like math, IO,
and common functional techniques. Check the `doc/` directory for the time being
for example functions.

### Syntactic Sugar

I would like to add two forms of syntactic sugar though, `let` expressions:

    let x = E1 in E2

Is actually sugar for:

    (fun x. E2) E1

And multi variable functions (which can still be curried, from left to right):
    
    fun x,y,z. E

Is actually sugar for:
    
    fun x. (fun y. (fun z. E))

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

    ./fib.el


### Examples

What better way to explain a language than to provide some examples. The 
following are a few popular examples to demo ErLam. (Note the canonical hello
world is not possible as this language does not have strings.)

#### Example program: Fibonacci

Not parallelized, but a simple Fibonacci function with continuation.

    (fun n. (rep n) (fun f.
                (fun a. (fun b. (f b (add a b))))) id 0 1)     

To parallelize, it's a bit more difficult as we need to synchronize using 
channels. The following function spawns two new sub-processes for both numbers
and then waits for their return:

    (fun n. (fun r. r r)
            (fun f. (fun m. if (lt m 2) m (merge (fun _.f f (sub m 1))
                                                 (fun _.f f (sub m 2))
                                                 add)))
            n)

Here we use the merge function, which spawns the last two functions as separate
processes and then merges their return values via the provided `add` function.
As we can't directly access an evaluated spawned process, `merge` wraps the call 
in a channel. Here's the `merge` function:

    (fun a. 
        (fun x. (ignore (spawn (fun _.swap x (eval a))))
                (fun b. 
                        (fun y. (ignore (spawn (fun _. swap y (eval b))))
                                (fun m. m (swap x nil) (swap y nil))
                        ) newchan)
        ) newchan)
        
#### Example program: Sieve of Eratosthenes

This is a popular example to find the n'th prime number. The following example
works by generating a server with filter processes which determine if a number
is prime or not.

-- TODO --




