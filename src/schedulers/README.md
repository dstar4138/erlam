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
  a process to reduce from another scheduler.

* _The Single-Threaded Dual-Queue CML Scheduler_ - Based on the CML's dual-queue
  design, this scheduler breaks up processes into `computation` and 
  `communication` bound queues. This attempts to improve the Interactivity of
  the system.

* _The Multi-Threaded Dual-Queue CML Scheduler_ - Based on the previous, but
  adds a work-stealing element to it (see MTRRWS above).


More scheduler's will obviously follow, but it is my intention to build off of
this set of schedulers.

