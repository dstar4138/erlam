## Multi-Core Round-Robin Scheduler on a Global Queue ##

* Spawns to the end of the global queue.
* Picks from the top of the global queue, returns to the bottom.
* Reduces for a set amount before next pick. Default reduction count is 20.
* If queue is empty and process and continue to reduce, reset reduction count 
  and continue.

This is a very simplistic and poorly executing scheduler for testing a 
multi-core RTS implementation. For a single core representation, see 
`erlam_sched_single.erl`, which maintains a single FIFO queue for all processes.

