## Cooperative Scheduler Designs

Please see my [Thesis Blog](http://cs.rit.edu/~ard4138/blog-posts/update3) for 
more information on this topic!

### Introduction

Most of the reasoning behind developing my own compiler was to visualize the 
behaviour of a scheduling system on an interesting subset of example simulation
applications. I got it in my head that the scheduling systems my research had 
turned up were missing something critical, namely cooperativity of concurrent
processes. 

### The Schedulers:

I turn my attention to distilling cooperativity based mechanisms out into their
own schedulers. Below are some descriptions for the examples accompanying this
framework:

#### CML + Occam-Pi = Longevity-Batching

CML's primary feature was this recognition of long-running and short-running
processes (or computation and communication-bound processes respectively). This
is useful in the recognition of tasks which should be parallelized. Batching, it
might be safe to say, would only be useful for processes which need to 
communicate fairly frequently (*i.e.* short-running processes). Thus our process
queue is now a queue-of-queues where long-running processes are singleton 
batches. Work stealing takes place at the batch level while still observing the
channel implementation (note adding Channel Absorption achieves cooperativity
awareness).


#### Bipartite Graph Optimizations & Smart Sorting

Note that the set of processes to the set of channels can be represented as a 
bipartite graph. An optimization can be performed on the set of owned processes 
and channels to order based on similar channel usage. This can be implemented
as an actual graph structure, a set of bloom-filters tagged to each process, 
etc.

Similarly to batching, it allows for smarter stealing as another scheduler may 
successively pick cooperating processes of the end. By doing so, if ever 
another process wishes to acquire more work, it will be able to pull off more 
than one process and know with high certainty that these processes are related.


#### Channel Pinning & Smart Stealing

Working in the opposite direction, we can, on channel creation attempt to pin
channels to the logical-processing units in a uniform way which can also take
into account the channels' usage frequency and popularity. Structuring the
channel pinning as a tree to match the NUMA based cache representation may have
an added bonus too.

Work-Stealing acts like Go-Fish in requesting work based on particular channel
preferences. As this is time-consuming, we limit this stealing method to 
lock-free shared-queue operations.


### The Channel Implementations

#### Process Absorption

When a process yields to a communication, it may either get taken by the channel
to be returned to the process which frees it, or it can just return as a halted
process. In the latter case, the process stays on the same core and in the same
process queue. In the prior case, the process is guaranteed to be relocated to
the same channel which it communicates with.

Note however, both do not immediately guarantee great results in all cases, nor
with every channel implementation. In the case of ErLam's swap channels on a 
global or single queue, a channel has no need to reorder a process as it is 
guaranteed that if there is a swap waiting to happen further down the queue, it
will happen before arriving back at the blocked process. In fact, by 
implementing absorption, it may introduce an unneeded overhead.

Also note that the previously stated options are not the only actions to be 
taken upon a voluntary yield, it is entirely possible that the scheduler 
utilizes another mechanism for segregating these blocked processes from the 
rest of the process queue. However, these show how the recognition of 
cooperativity can effect channel implementation decisions.

Questions:
- How do swap channels effect Absorption?
- Is there an Overhead due to frequent swaps? 
    - Typically for send/recv channels, so there are queues involved and 
      performed less frequently.
- Is there a more effective method for swaps which has the same effect?

