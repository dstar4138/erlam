-record(process, {
   %% A Process' state is either running or blocked. If a scheduler pre-empts
   %% a process, it is still technically running, if a process is in the 
   %% queue in the running state, it can be considered a 'computational' thread
   %% for all intents and purposes.
   state  :: running | blocked,

   %% In the event this process is a 'primary' thread, it will need to return
   %% the value of it's evaluation upwards into the runtime system. This can
   %% be checked upon completion, if nil, then its value is discarded otherwise
   %% it signals the end of the program.
   resrep = nil :: nil | pid(),

   %% This is the stored expression to be evaluated in each instance of the 
   %% scheduler. This should be seen as a black box as far as the scheduler
   %% implementation is concerned. As in a typical rts, it is difficult to
   %% pre-evaluate the next step without actually doing it. 
   exp    :: term(),
   env=[] :: [{atom(),term()}],

   %% Hangtime and initial timestamp. When a process randomly asks to be
   %% rescheduled at a later time. Note the 'hangtime' is thus a suggested
   %% minimum (unless the particular scheduler implementation makes an extra
   %% effort, such as a halt-the-world approach).
   hang = {nil, nil} :: { nil | erlang:timestamp(), nil | pos_integer() },

   %% Schedulers may want to store internal state in a process for quick 
   %% evaluation. This is the feild where it will be stored.
   notes  :: any(),

   %% Make a unique reference to this process.
   proc_id = make_ref()
}).
-define(new_process(F,E),#process{state=running,exp=F,env=E,resrep=nil}).
-define(is_blocked(P),P#process.state==blocked).
-define(is_running(P),P#process.state==running).
-define(step(P),P#process{exp=erlam_rts:safe_step(P#process.exp)}).
-define(get_notes(P),P#process.notes).
-define(set_notes(P,N),P#process{notes=N}).
-define(is_primary(P),P#process.resrep/=nil).
-define(set_primary(P),P#process{resrep=self()}).
-define(set_hangtime(P,T,H),P#process{hang={T,H}}).
-define(get_timestamp(P),element(1,P#process.hang)).
-define(get_hangfortime(P),element(2,P#process.hang)).

-type erlam_process() :: #process{}.
