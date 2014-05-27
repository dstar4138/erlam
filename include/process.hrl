-record(process, {
   %% A Process' state is either running or blocked. If a scheduler pre-empts
   %% a process, it is still technically running, if a process is in the 
   %% queue in the running state, it can be considered a 'computational' thread
   %% for all intents and purposes.
   state  :: 'RUNNING' | 'BLOCKED',

   %% In the event this process is a 'primary' thread, it will need to return
   %% the value of it's evaluation upwards into the runtime system. This can
   %% be checked upon completion, if nil, then its value is discarded otherwise
   %% it signals the end of the program.
   resrep :: nil | pid(),

   %% This is the stored expression to be evaluated in each instance of the 
   %% scheduler. This should be seen as a black box as far as the scheduler
   %% implementation is concerned. As in a typical rts, it is difficult to
   %% pre-evaluate the next step without actually doing it. 
   exp    :: term(),
   env=[] :: [{atom(),term()}],

   %% Schedulers may want to store internal state in a process for quick 
   %% evaluation. This is the feild where it will be stored.
   notes  :: any()
}).
-define(new_process(F,E),#process{state='RUNNING',exp=F,env=E,resrep=nil}).
-define(is_blocked(P),P#process.state=='BLOCKED').
-define(is_running(P),P#process.state=='RUNNING').
-define(step(P),P#process{exp=erlam_rts:safe_step(P#process.exp)}).
-define(get_notes(P),P#process.notes).
-define(set_notes(P,N),P#process{notes=N}).
-define(is_primary(P),P#process.resrep/=nil).
-define(set_primary(P),P#process{resrep=self()}).

-type erlam_process() :: #process{}.
