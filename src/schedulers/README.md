## The ErLam Schedulers ##

I hope to provide a decent number of robust schedulers to get a decent 
comparison amongst the implementations. This README will provide a high-level
listing of each, to see more detail, see their subsequent bundled README.


* _The Single-Threaded Round-Robin Scheduler_ - Uses a single FIFO queue for
  which all processes are spawned to. There is no rearrangement of order, and
  the single-core/thread scheduler with round-robin the queue, performing
  a set number of reductions on each process.

* _The Multi-Threaded Round-Robin Global-Queue Scheduler_ - A multi-core version
  of the previous scheduler. This uses a single global queue which all 
  schedulers share. All waiting schedulers queue themselves and continuously
  check the shared global queue.

* _The Multi-Threaded Round-Robin Work-Stealing Scheduler_ - An improvement on
  the previous scheduler. Instead of a global queue, each scheduler maintains
  their own. A waiting scheduler will randomly sleep-and-steal until it finds 
  a process to reduce from another scheduler. The provided implementation gives
  two example stealing mechanisms for comparison.

* _The Single-Threaded Dual-Queue CML Scheduler_ - Based on the CML's dual-queue
  design, this scheduler breaks up processes into `computation` and 
  `communication` bound queues. This attempts to improve the Interactivity of
  the system.

* _The Multi-Threaded Dual-Queue CML Scheduler_ - Based on the previous, but
  adds a work-stealing element to it (see MTRRWS above). However, this one has
  no basis in current scheduler designs and is thus being ignored for the time
  being.

The following are the set of cooperativity aware schedulers. Each use a
different mechanic so comparisons can be made. Note all are multi-threaded
unless stated otherwise:

* _Longevity-Based Batching Scheduler_ - Attempts to recognize whether processes
  are long-running processes or short-running and by doing so, aid in the
  batching of short-running processes which may communicate with one another.

* _Bipartite Graph Aided Queue Shuffling Scheduler_ - Instead of using a
  mechanic to maintain close coupling over time, this scheduler uses the idea
  of program phases. When it guesses the application has started a phase shift
  it will attempt to reorder the process queue to be more efficient.

* _Channel-Pinning Scheduler_ - Working in an opposite direction, this
  scheduler tries forcing processes not to compute swaps unless they are
  first sent to the appropriate core. Every time a new channel is created it
  is automatically pinned to the next least used core. Stealing in this
  scheduler is more creative as it prefers stealing processes which have
  previously communicated with a preferred channel.

