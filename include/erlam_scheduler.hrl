%%% ==========================================================================
%%% Scheduler Type Checking
%%% ==========================================================================

%%% The Scheduler can return a state which will be weaved through.
-type scheduler_state() :: any().

%%% The inter-process message queue which can be modified in-between steps.
-type message_queue() :: list( any() ).

%%% ErLam can auto handle warnings and errors based on runtime verbosity.
-type log_msg() :: string() | atom() | iodata().

%%% The processor ID that the scheduler is bound to.
-type proc_id() :: unbound | non_neg_integer().

%%% The layout perscribed to the particular CPU topology of the system by 
%%% this particular scheduler module. Thus we could have multiple layouts based
%%% on options and hardware layouts.
%%%     [ { ProcID, Primary, SchedulerBehaviourModule, OptionsForInit }, ... ]
-type scheduler_desc() :: {non_neg_integer(), boolean(), atom(), scheduler_opts()}.
-type scheduler_layout() :: [ scheduler_desc() ]. 
                            
%%% Options that are passed into the scheduler at runtime are as follows:
-type scheduler_opt() :: 
    % The processor binding out of the total number of processors.
    {processor, proc_id()}             |
    {total_procs, non_neg_integer()}   |
    % The processor which will be considered primary (start the computation).
    {primary, proc_id()}               |
    % Whether it's a debug run.
    {debug, boolean()}                 |
    % How long between steps will the scheduler hang if no messages recieved.
    {message_hang, integer()}          |
    % Max number of messages to add to the buffer between steps. -1 is all.
    {message_buffer, integer()}.
    %TODO: We also need options for throttling statistics gathering.

-type scheduler_opts() :: list( scheduler_opt() ).

-type scheduler_status() :: 'WAITING' | 'RUNNING' | 'BLOCKED'.

