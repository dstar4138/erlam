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

% The num of reductions before moving to the next proc
-define(MAX_REDUCS, 20).

% Threshold for when processes get requeued or pushed back.
-define(REDUC_THRESHOLD, 0.1). % (Default 10%)

% Conservative batch size high-end limit. Only checked upon spawns. Could also
% be heuristically decided based on channel usage averages (i.e. the average
% number of unique processes a channel sees could be the max batch size).
-define(BATCH_SIZE_MAX, 20). 

% It may be useful to run through the batch multiple times before asking for a
% new one. The batch replay constant multiplies with the number of processes
% currently in the queue to produce the number of 'rounds' to run before giving
% up the batch.
-define(BATCH_REPLAY, 1).

%% Process inspection operations:
-include("process.hrl").

%%% Internal Scheduler State Object %%%
-record(state, {
% Current Process Status.
 cur_proc = nil, 
 cur_reduc = 0,

% Current Batch information.
 grading = false, % Are we grading the cur_proc for longevity?
 loading_dock = nil, % Current batch being worked on.
 rounds = 0, % Number of rounds (processes) to run before getting a new batch.

% Current Scheduler Configuration. 
 procs = nil, % Reference to personal Queue-of-Queues.
 max_reduc = ?MAX_REDUCS, % User defined max reductions per process execution.
 batch_replay = ?BATCH_REPLAY, % User defined constant to find the round count.
 max_batch = ?BATCH_SIZE_MAX, % User defined max batch size.
 tick_fun = nil, % Reference to tick function based on work-stealing choice.
 waiting = false % Whether we are currently in thief mode.
}).

%%% State logging
-define(PROC(P), case P of nil -> "nil"; 
                           _ ->io_lib:format("P<~p>",[P#process.proc_id])
                 end).
-define(LD(LD), case LD of nil -> "nil";
                           _ -> io_lib:format("Q<~p>",[queue:len(LD)])
                end).
-define(STATE2STR( S ), io_lib:format("{state,(w:~p,g:~p),~s,~p:~p,~s:~p}",
                            [S#state.waiting,
                             S#state.grading,
                             ?PROC(S#state.cur_proc),
                             S#state.cur_reduc,S#state.max_reduc,
                             ?LD(S#state.loading_dock),
                             S#state.rounds])).

%%% ==========================================================================
%%% Erlam Scheduler Public API
%%% ==========================================================================

%% @doc Return the Scheduler topology for this scheduler implementation.
layout( Topology, Options ) ->
    erlam_scheduler:layout( ?MODULE, Topology, Options ).

%% @doc Initializing the scheduler on the processor by checking user defined
%%   option overrides and creating the queue process and the rest of the 
%%   internal scheduler state.
%% @end 
init( Options ) -> make_state( Options ).

%% @doc Cleanup is simple too, we ignore unfinished processes in the queue.
cleanup( _State ) -> ok.

%% @doc As there are two methods for ticking, this method extracts it from
%%  State and runs it with the given function.
%% @end
%% @see tick_shared_queue/2.
%% @see tick_interrupt_steal/2.
tick( Status, #state{tick_fun=F} = State ) -> F( Status, State ).

%% @doc Spawn a process to the local queue by pushing back the parent process
%%   into the current batch so we can test the uncoming process to see if it is 
%%   long or short-running. Note it takes the current process's reductions into
%%   account when pushing back. If the reduction count is less than 10% of the
%%   max, we enqueue it to the end otherwise at the front.
%% @end
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
spawn_process( Process, #state{ cur_proc=C, cur_reduc=R, max_reduc=MR, 
                                 loading_dock=Q, rounds=Rounds}=State ) -> 

    %% First choose which side of the loading dockto put the current process.
    Batch = case R =< (MR*?REDUC_THRESHOLD) of
                  true -> queue:in( C, Q );
                  false-> queue:in_r( C, Q )
            end,
    %% Then decide whether we are at our max loading dock size.
    {NewBatch,NewRounds} = 
            case queue:len( Batch ) > State#state.max_batch of
                true ->
                    erlam_private_queue:push(Batch,State#state.procs),
                    {queue:new(), State#state.batch_replay};
                false -> {Batch, Rounds+1}
            end,
    %% Finally update our state based on our decisions.
    {ok, State#state{ cur_proc = Process,
                      cur_reduc = MR,
                      grading = true,
                      loading_dock = NewBatch,
                      rounds = NewRounds,
                      waiting = false }}.

%%% Tick Functions:

%% @doc Tick on a shared queue, which means, as long as our queue is non-empty
%%   we can leave the stealing up to the private queue and just worry about 
%%   how we round-robin. However, in the event we are 'waiting' we need to 
%%   randomly try to steal from another LPU.
%% @end
tick_shared_queue( startup, State ) -> pick_new_sq( State );
tick_shared_queue( waiting, State ) -> steal_process_sq( State );
tick_shared_queue( running, #state{cur_reduc=0} = S ) -> 
    pick_new_sq( mark_if_grading( S ) );
tick_shared_queue( running, #state{cur_reduc=_} = S ) -> reduce( S ).

%% @doc  Tick based on an interruption. At each preemption or yield we'll 
%%   check our message queue for a theif message. In the event of its 
%%   existence we'll respond to their steal.
%% @end
tick_interrupt_steal( startup, State ) -> pick_new_is( State );
tick_interrupt_steal( waiting, State ) -> steal_process_is( State );
tick_interrupt_steal( running, #state{cur_reduc=0} = S ) -> 
    pick_new_is( mark_if_grading( S ) );
tick_interrupt_steal( running, #state{cur_reduc=_} = S ) -> reduce( S ).


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
    case proplists:get_value( shared_queue, Options, false ) of
        true -> % We chose to use a personal, but shared queue:
            TickFun = fun tick_shared_queue/2,
            {ok, #state{tick_fun=TickFun,
                        procs=QueueID,
                        max_batch=MaxBatch,
                        batch_replay=BatchReplay, 
                        max_reduc=MaxReducs}};
        false -> % We chose to use theif process emulation:
            TickFun = fun tick_interrupt_steal/2,
            {ok, #state{tick_fun=TickFun,
                        procs=QueueID,
                        max_batch=MaxBatch,
                        batch_replay=BatchReplay,
                        max_reduc=MaxReducs}}
    end.

%%% ==========================================================================
%%% Work-Stealing Functionality
%%% ==========================================================================

%% @hidden
%% @doc Randomly pick a LPU ID from the list of IDs
pick_random_lpu( _, [] ) -> 0;
pick_random_lpu( M, ListOfIDs ) ->
    R = lists:nth(random:uniform(length(ListOfIDs)), ListOfIDs),
    case R == M of
        false -> R; 
        true -> pick_random_lpu( M, ListOfIDs )
    end.

%% @hidden
%% @doc Loads a new process into the cur_proc slot based on the loading dock
%%   selection first, otherwise it will check it's own queue for a new batch
%%   and then set itself as waiting so it can steal.
%% @end
pick_new_sq( #state{loading_dock=nil,procs=Queue} = State ) ->
    case erlam_private_queue:pop( Queue ) of
        {ok, ProcessBatch} -> {ok, running, resetCur( ProcessBatch, State )};
        false -> {ok, waiting, State}
    end;
pick_new_sq( #state{cur_proc=C,procs=Ps,loading_dock=LD,rounds=0} = State ) ->
    case enqueue_then_pop_push( C, LD, Ps ) of
        {ok, ProcessBatch} ->
            {ok, running, resetCur( ProcessBatch, State )};
        false -> {ok, waiting, blankOut(State)}
    end;
pick_new_sq( #state{cur_proc=nil,loading_dock=LD,rounds=R,procs=Ps}=State ) ->
    case queue:len( LD ) of
        0 -> 
            (case erlam_private_queue:pop( Ps ) of
                 false -> {ok, waiting, blankOut(State)};
                 {ok,NewLD} -> {ok, running, resetCur( NewLD, State )}
             end);
        _ -> 
            NewState = resetCur( LD, State ),
            {ok, running, NewState#state{rounds=R-1}}
    end;
pick_new_sq( #state{cur_proc=C,loading_dock=LD,rounds=R,procs=P}=State ) ->
    Batch = case isMarked(C) of 
                true -> % Push the Long-running process as a new batch
                    Singleton = queue:from_list([C]),
                    ok = erlam_private_queue:push(Singleton,P),
                    LD; % Return current Batch as new Batch.
                false ->
                    queue:in( C, LD )
            end,
    NewState = resetCur( Batch, State ),
    {ok, running, NewState#state{ rounds=R-1 }}.

%% @hidden
%% @doc Pick a random LPU and randomly attempt a steal from them by accessing
%%   their private queue directly. (We steal from the bottom of the queue).
%% @end
steal_process_sq( State ) ->
    MyID = erlam_sched:get_id(),
    IDs = erlam_sched:get_ids(),
    LPUID = pick_random_lpu( MyID, IDs ),
    case erlam_private_queue:steal( LPUID ) of
        {ok, ProcessBatch} -> {ok, running, resetCur(ProcessBatch, State)};
        false -> {ok, waiting, State}
    end.

%% @hidden
%% @doc Check for a theif process, if it exists, then run it first. Then
%%  look at private queue to update our current process, however ignore it
%%  if we currently have nothing to do.
%% @end
pick_new_is( #state{loading_dock=nil} = State ) -> pick_new_sq( State );
pick_new_is( State ) -> pick_new_sq( check_for_theif( State ) ). 

%% @hidden
%% @doc Check if the message queue has a theif message. If it does, then 
%%   perform a possible steal (only fails if queue is empty and ignores
%%   current process).
%% @end
check_for_theif( State ) ->
    case erlam_sched:check_mq( false ) of
        false -> State; % ignore
        {ok, {spawn, nil}} -> State; % ignore
        {ok, {spawn, Process}} -> % add process to queue 
            resetCur( Process, State );
        {ok, {steal, LPU}} -> % return a process batch to LPU
            (case erlam_private_queue:pop( State#state.procs ) of
                 false ->
                     erlam_sched:send( LPU, {spawn, nil} );
                 {ok, Top} ->
                     erlam_sched:send( LPU, {spawn, Top} )
            end), 
            State;
        Other -> 
            ?DEBUG("Other: ~p~n",[Other]), 
            State
    end.

%% @hidden
%% @doc Stealing a process comes in two steps. First if not waiting send a 
%%   random LPU a theif message and set mode to waiting. Next, wait for 
%%   response from LPU when they hit the theif thread.
%% @end
steal_process_is( #state{ waiting=true } = State ) ->
    case erlam_sched:check_mq( true ) of %hang until return
        {ok, {steal, LPU}} ->
            erlam_sched:send( LPU, {spawn, nil} ),
            {ok, waiting, State};
        {ok, {spawn, nil}} ->
            {ok, waiting, State#state{waiting=false}};
        {ok, {spawn, ProcessBatch}} ->
            NewState = resetCur(ProcessBatch,State),
            {ok, running, NewState#state{waiting=false}};
        _Other -> 
            {ok, waiting, State}
    end;
steal_process_is( State ) ->
    MyID = erlam_sched:get_id(),
    LPUID = pick_random_lpu( MyID, erlam_sched:get_ids() ),
    erlam_sched:send( LPUID, {steal, MyID}),
    {ok, waiting, State#state{waiting=true}}.


%%% ==========================================================================
%%% Process Functionality
%%% ==========================================================================
%%% Note that we utilize the built in process 'notes' field, which is used to
%%% store scheduler information. We set this to true if it's been marked as a
%%% communication bound thread.

%% @hidden
%% @doc Mark the Process as Long-Running
-spec markTid( #process{} ) -> #process{}.
markTid( Proc ) -> Proc#process{notes=true}.

%% @hidden
%% @doc Mark the Process as Short-Running.
-spec unmarkTid( #process{} ) -> #process{}.
unmarkTid( Proc ) -> Proc#process{notes=false}.

%% @hidden
%% @doc Is the process marked as Long-Running.
-spec isMarked( #process{} ) -> boolean().
isMarked( #process{notes=N} ) -> (N==true).

%% @hidden
%% @doc If the current state is in grading mode, mark the current process
%%   as long running and return the new state. We pipe-line this before 
%%   picking a new process from the loading dock. Which means this could
%%   force the process to become a new singleton batch.
%% @end
mark_if_grading( #state{ cur_proc=nil } = State ) -> State;
mark_if_grading( #state{ grading=true, cur_proc=C } = State ) ->
    State#state{cur_proc=markTid(C), grading=false};
mark_if_grading( State ) -> State.

%% @hidden
%% @doc Perform a reduction.
reduce( #state{ cur_proc=P, cur_reduc=R, loading_dock=LD } = State ) ->
    case erlam_rts:safe_step(P) of
        {ok,NP} -> {ok, running, State#state{ cur_proc=NP, cur_reduc=R-1 }}; 
        {stop,NP} -> check_on_stop( NP, State );
        {blocked, NPs} -> %% We are swaping, so ask for a new proc, in meantime.
            NewLD = insert_loading_dock( NPs, LD ),
            {ok, running, State#state{ loading_dock=NewLD,
                                       cur_proc=nil, cur_reduc=0}};
        {unblocked, [H|T]} ->
            NewLD = insert_loading_dock( T, LD ),
            {ok, running, State#state{ loading_dock=NewLD, 
                                       cur_proc=H, cur_reduc=R-1 }};
        {hang, NP} -> %Ignore
            {ok, running, State#state{ cur_proc=NP, cur_reduc=0 }};
        {error, Reason} -> exit( Reason ) % Won't handle errors
    end.
check_on_stop( Process, State ) ->
    case ?is_primary( Process ) of
        %% If it finished and was the primary process, return it!
        true -> 
            erlam_sched:return( Process ), %will handle unwraping
            {stop, State};
        %% If it finished and was not primary, then drop it and move to next
        false ->
            {ok, running, State#state{ cur_proc=nil, cur_reduc=0 }}
    end.

%% @hidden
%% @doc Insert a set of processes into the local queue.
insert_loading_dock( [], P ) -> P;
insert_loading_dock( [H|T], P ) ->
    Unmarked = unmarkTid( H ),
    NewBatch = queue:in( Unmarked, P ), 
    insert_loading_dock(T, NewBatch).

%% @hidden
%% @doc Hide possible channel absorption issues or empty loading docks by
%%  calling pop rather than pop_push, which would hand us back an empty queue.
%% @end
enqueue_then_pop_push( nil, LD, Ps ) ->
    case queue:len( LD ) of
        0 -> erlam_private_queue:pop( Ps );
        _ -> erlam_private_queue:pop_push( LD, Ps )
    end;
enqueue_then_pop_push( CurProc, LD, Ps ) ->
    Batch = case isMarked( CurProc ) of
                true ->
                    Singleton = queue:from_list([CurProc]),
                    ok = erlam_private_queue:push( Singleton, Ps ),
                    LD;
                false -> 
                    queue:in( CurProc,LD )
            end,
    erlam_private_queue:pop_push( Batch, Ps ).

%% @hidden
%% @doc Resets the current process in the event we have a new batch. This
%%   also resets the reductions and round counters.
%% @end
resetCur( Batch, #state{ max_reduc=MR } = State ) ->
    {{value,Top},NewBatch} = queue:out(Batch),
    Rounds = queue:len(Batch) * State#state.batch_replay,
    State#state{cur_proc=Top, cur_reduc=MR,
                loading_dock=NewBatch, rounds=Rounds}.

%% @hidden
%% @doc Blanks out a state in the event of completely stolen processes or
%%   when a large set of processes stop.
%% @end
blankOut( State ) ->
    State#state{ cur_proc=nil, cur_reduc=0,
                 loading_dock=nil, rounds=0 }.

