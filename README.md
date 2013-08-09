# ErLam: Erlang Lambda Compiler

NOTE: Currently in alpha and there are no instructions on how to use.

Erlam is a compiler for an experimental derivative of Lambda Calculus with 
integers, and swap channels. It's being geared for testing various scheduling 
systems. We chose the bare necessities for our language and are avoiding all 
boilerplate with minimum syntax sugar.

    Expression ::= x             # Variables
                 | n             # Integers
                 | (E)           # Expressions can be grouped
                 | E E           # Application of expressions
                 | if E E E      # If clause on natural numbers, true=!0
                 | swap C E      # Blocking Swap on channel C with value E
                 | spawn E       # Spawn process to handle expression E, return id
                 | fun x.E       # Function Definition (single variable)

This is the extent of the language. Of course there are also a handful of 
internal functions that can be used to simplify some things:

    print -> Prints x to stdout, returns x.
    me -> An integer which represents the current process id.
    rep -> unwraps an integer into its lambda equivalent (kinda like a for loop)
    lt, gt, leq, geq, eq -> check if first number is <,>,<=,>=,== to the second.
    dec, inc -> --,++ an integer.
    add, sub, max, min, mul, exp -> maths
    id -> returns x.
    merge -> sync two spawns and combine with a function.
    comm -> Wrap a spawn with a channel pass and return the channel
    ret -> Wrap a comm in a swap call.

Example definitions of these functions can be seen in the `doc` directory during
this construction phase.

## Example program: Fibonacci

Not parallelized, but a simple Fibonacci function with continuation.

    (fun n. (rep n) (fun f.
                (fun a. (fun b. (f b (add a b))))) id 0 1)     

To parallelize, it's a bit more difficult without sequentializing subsequent 
which is why we can sync using channels. In pseudo code it would look like:

    fib n = if n<2 then n else ret (f n)
    f( n, c) = swap c (if n < 2 then n
                                else merge add (f (n-1)) (f (n-2)) )

## Syntactic Sugar

I would like to add two forms of syntactic sugar, `let` expressions:

    let x = E1 in E2

Is actually sugar for:

    (fun x. E2) E1

And multi variable functions (which can still be curried):
    
    fun x,y,z. E

Is actually sugar for:
    
    fun z. (fun y. (fun x. E))

Both expressions are quickly converted to their non-sugar forms after parsing.

