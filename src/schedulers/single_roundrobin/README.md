## Single-Thread Round-Robin Scheduler ##

* Spawns to the end of the queue. 
* Picks from the top, returns to bottom.
* Reduces for a set amount before next pick. Default reduction count is 20.

This is a very simple scheduler for testing the RTS implementation. The 
multi-core version is the `erlam_sched_global.erl`, which maintains only a 
single global queue and all schedulers perform the above actions.

