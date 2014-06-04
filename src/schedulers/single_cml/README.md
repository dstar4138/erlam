## Single-Thread Dual-Queue CML-Based Scheduler ##

* Spawns to the end of the primary queue. 
* Picks from the top of the primary queue, returns to bottom.
* Reduces for a set amount before next pick. Default reduction count is 20.
* After phase count is finished, repopulate primary queue

