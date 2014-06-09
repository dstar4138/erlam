## Multi-Thread Round-Robin Work-Stealing Scheduler ##

* Spawns to the end of the queue. 
* Picks from the top, returns to bottom.
* Reduces for a set amount before next pick. Default reduction count is 20.
* If queue is empty, randomly steal from another's queue.

This is a very simple scheduler for testing the RTS inter-thread communication 
implementation. Note that the only interesting bit is the `private_queue` 
implementation, where it binds itself to the same LPU that the scheduler that
started it is running.

Note: there are two methods for processing a steal which can be adjusted using
the command line options listed below:

* `interrupt_steal` - Default, steals by sending a message to the queue which
  is handled after every preemption or yield. This is synonymous with how 
  languages such as Manticore spawn an atomic thief process to send a process
  back over.

* `shared_queue` - Allows other scheduler threads the ability to synchronize
  around the random other queue they are stealing from. This is seen in 
  languages like Occam-Pi, which use a lock-free doubly-ended queue (take
  from front, spawn and steal to back).

You can specify these options by performing something like the following:

    ./example/pfib.ex -s erlam_sched_multi_ws -- shared_queue

