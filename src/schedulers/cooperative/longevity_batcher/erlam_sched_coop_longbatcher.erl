%%% A Multi-Core Longevity-Based Batching Feedback Scheduler
%%%
%%% This scheduler utilizes process cooperativity to attempt optimizions for 
%%% cache locality and parallelism using a CML like process recognition. It 
%%% does so by providing a single mechanism for possible comparison:
%%%
%%%     - Selective Process Batching: Short-running processes are forced
%%%         to reside in batches which can be stolen as a whole. Long-running
%%%         processes are singleton-batches.
%%%
%%%     - Note turning on Channel Absorption will allow the batches to be
%%%         reconstructed according to cooperating processes. Without it,
%%%         batching only takes causal relationships into account.
%%%
%%%     - This scheduler implements on top of the work-stealing scheduler
%%%         so the same options can be applied (i.e. interrupt_steal or
%%%         shared_queue).
%%%
-module(erlam_sched_coop_longbatcher).
-behaviour(erlam_scheduler).

%% General Debugery.
-include("debug.hrl").

%% Erlam Scheduler API
-export([ layout/2, init/1, cleanup/1, tick/2, spawn_process/2 ]).
-export([ options/0 ]).

%% Process inspection operations:
-include("process.hrl").
-include("longbatcher_state.hrl").

%%% ==========================================================================
%%% Erlam Scheduler Public API
%%% ==========================================================================

%% @doc Return the Scheduler topology for this scheduler implementation.
layout( Topology, Options ) ->
    %observer:start(),
    erlam_scheduler:layout( ?MODULE, Topology, Options ).

%% @doc Initializing the scheduler on the processor by checking user defined
%%   option overrides and creating the queue process and the rest of the 
%%   internal scheduler state.
%% @end 
init( Options ) -> 
    make_state( Options ).

%% @doc Cleanup is simple too, we ignore unfinished processes in the queue.
cleanup( _State ) -> ok.

%% @doc As there are two methods for ticking, this method extracts it from
%%  State and runs it with the given function.
%% @end
%% @see longbatcher_preempts
tick( startup, #state{preempt_fun=F}=State ) -> F( startup, State );
tick( waiting, #state{steal_fun=F}=State ) -> F( State );
tick( running, #state{cur_reduc=0, preempt_fun=F}=State ) -> F(running, State);
tick( running, State ) -> reduce( State ).

%% @doc Catch the event when we are primary and get first process. 
%%  Alternatively call the user-specified spawn function.
%% @end
%% @see longbatcher_spawns
spawn_process( Process, #state{cur_proc=nil, loading_dock=OldBatch}=State ) ->
    %% The scheduler currently has no process. Either waiting on a steal, 
    %% or in start up mode, or has an empty batch due to process absorption. 
    %% We reset the old batch with this new process.
    NewBatch = case OldBatch of nil -> queue:new(); _ -> OldBatch end,
    Rounds = (queue:len(NewBatch)+1)*State#state.batch_replay,
    {ok, State#state{ cur_proc = Process,
                      cur_reduc = State#state.max_reduc,
                      grading=false,
                      loading_dock = NewBatch,
                      rounds = Rounds,
                      waiting = false }};
spawn_process( P, #state{spawn_fun=Spawn}=S ) -> Spawn( P, S ).

%%% ==========================================================================
%%% Optional ErLam Scheduler API
%%% ==========================================================================

%% @private
%% @doc Returnsthe arguments to pass in to the scheduler as a direct printout.
options() ->
    {ok, 
  "max_reduc - The number of reductions on an expression before pick_next.\n"++
  "max_batch - The max size a batch can be due to a spawned process.\n"++
  "batch_replay - The number of times to replay batches before getting another.\n"++
  "pushback_batcher - Keep the spawning process with the newly spawned "++
        "process during batch overflow.\n"++
  "interrupt_steal - Default, switches scheduler to use theif process emulation.\n"++
  "shared_queue - Switches scheduler to use steal directly from other cores."
    }.

%% @hidden
%% @doc After parsing, update init state with user-changes.
make_state( Options ) ->
    {A,B,C} = erlang:now(), random:seed(A,B,C),
    MaxReducs = proplists:get_value( max_reduc, Options, ?MAX_REDUCS ),
    MaxBatch = proplists:get_value( max_batch, Options, ?BATCH_SIZE_MAX ),
    BatchReplay = proplists:get_value( batch_replay, Options, ?BATCH_REPLAY ),
    {ok, QueueID} = erlam_private_queue:start_link(),
    State = build_functionality( Options ),
    {ok, State#state{ procs=QueueID,
                      max_batch=MaxBatch,
                      batch_replay=BatchReplay,
                      max_reduc=MaxReducs}}.

build_functionality( Options ) ->
    {PreemptFun,StealFun} = get_ws_type( Options ),
    #state{preemption_type=get_preempt_type( Options ),
           preempt_fun=PreemptFun,
           steal_fun=StealFun,
           spawn_fun=get_spawnfun( Options ),
           yield_fun=fun longbatcher_yields:default/3 %TODO: Only one currently.
          }.

get_preempt_type( Options ) ->
    case proplists:get_value( pt, Options, mark_if_grading ) of
        never_mark -> never_mark;
        always_mark -> always_mark;
        mark_if_grading -> mark_if_grading;
        N when is_integer(N) -> {mark_after, N};
        PT -> 
           io:format("ERROR: bad preemption-type '~p'.~n",[PT]),
           halt(1)
    end. 

get_ws_type( Options ) ->
    case proplists:get_value( shared_queue, Options, false ) of
        true -> % We chose to use a personal, but shared queue:
            {fun longbatcher_preempts:shared_queue/2
            ,fun longbatcher_steals:shared_queue/1};
        false -> % We chose to use theif process emulation:
            {fun longbatcher_preempts:interrupt_steal/2
            ,fun longbatcher_steals:interrupt_steal/1}
    end.

get_spawnfun( Options ) ->
    case proplists:get_value( pushback_batcher, Options, false ) of
        true -> fun longbatcher_spawns:push_back_batcher/2;
        false -> fun longbatcher_spawns:single_batcher/2
    end.

%%% ==========================================================================
%%% Process Functionality
%%% ==========================================================================
%%% Note that we utilize the built in process 'notes' field, which is used to
%%% store scheduler information. We set this to true if it's been marked as a
%%% communication bound thread.

%% @hidden
%% @doc Perform a reduction.
reduce( #state{ cur_proc=P, cur_reduc=R, yield_fun=Yield } = State ) ->
    case erlam_rts:safe_step(P) of
        {ok,NP} -> {ok, running, State#state{ cur_proc=NP, cur_reduc=R-1 }}; 
        {stop,NP} -> check_on_stop( NP, State );
        {blocked, NPs} -> Yield( blocked, NPs, State ); 
        {unblocked, NPs} -> Yield( unblocked, NPs, State );
        {hang, NP} -> %Ignore
            {ok, running, State#state{ cur_proc=NP, cur_reduc=0 }};
        {error, Reason} -> erlam_sched:error( Reason ) % Won't handle errors
    end.

%% @hidden
%% @doc Check if a process is primary on completion of evaluation. If it is
%%   then return it to the top-level runner, otherwise discard it silently.
%% @end
check_on_stop( Process, State ) ->
    case ?is_primary( Process ) of
        %% If it finished and was the primary process, return it!
        true -> 
            erlam_sched:return( Process ), % Will handle unwraping
            {stop, State};
        %% If it finished and was not primary, then drop it and move to next
        false ->
            {ok, running, State#state{ cur_proc=nil, cur_reduc=0 }}
    end.

