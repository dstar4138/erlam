%% An Erlam Multi Core Work Stealing Scheduler.
%%
%%  This is an example of a multi-core round-robin work-stealing scheduler. It
%%  uses a secondary Erlang process which binds itself to the same LPU to
%%  maintain the queue of processes for that scheduler. This is so that other
%%  scheduler's can contact it to steal from it without interfering with the
%%  operation of that scheduler.
%%
-module(erlam_sched_multi_ws).
-behavior(erlam_scheduler).

%% General Debugery.
-include("debug.hrl").
-include("process.hrl").

%% Erlam Scheduler API
-export([ layout/2, init/1, cleanup/1, tick/2, spawn_process/2 ]).
-export([ options/0 ]).

% The num of reductions before moving to the next proc
-define(MAX_REDUCS, 20).

-record( state, {  cur_proc = nil,
                   cur_reduc = 0,
                   max_reduc = ?MAX_REDUCS,
                   procs = nil,
                   tick_fun = nil,
                   waiting = false
                } ).

%%% ==========================================================================
%%% Erlam Scheduler Public API
%%% ==========================================================================

%% @doc Return the Scheduler topology for this scheduler implementation. This
%%   uses only Processor0, and will round robit on an internal work queue.
%% @end
layout( Topology, Options ) ->
    erlam_scheduler:layout( ?MODULE, Topology, Options ).

%% @doc Initializing the scheduler on the processor is easy as returning default
%%   state. The state will hold the local work queue and thats it.
%% @end
init( Options ) -> make_state( Options ).

%% @doc Cleanup is simple too, we ignore unfinised processes in the queue.
cleanup( _State ) -> ok.

%% @doc As there are two methods for ticking, this method extracts it from
%%  State and runs it with the given function.
%% @end
%% @see tick_shared_queue/2.
%% @see tick_interrupt_steal/2.
tick( Status, #state{tick_fun=F} = State ) -> F( Status, State ).

%% @doc Spawn a process based on method selected.
%% @see spawn_shared_queue/2.
%% @see spawn_interrupt_steal/2.
spawn_process( Process, #state{procs=P} = State ) -> 
    ok = erlam_private_queue:push( Process, P ),
    {ok, State}.

%%% Tick Functions:

%% @doc Tick on a shared queue, which means, as long as our queue is non-empty
%%   we can leave the stealing up to the private queue and just worry about 
%%   how we round-robin. However, in the event we are 'waiting' we need to 
%%   randomly try to steal from another LPU.
%% @end
tick_shared_queue( startup, State ) -> pick_new_sq( State );
tick_shared_queue( waiting, State ) -> steal_process_sq( State );
tick_shared_queue( running, #state{cur_reduc=0} = S ) -> pick_new_sq( S );
tick_shared_queue( running, #state{cur_reduc=_} = S ) -> reduce( S ).

%% @doc  Tick based on an interruption. At each preemption or yield we'll 
%%   check our message queue for a theif message. In the event of its 
%%   existence we'll respond to their steal.
%% @end
tick_interrupt_steal( startup, State ) -> pick_new_is( State );
tick_interrupt_steal( waiting, State ) -> steal_process_is( State );
tick_interrupt_steal( running, #state{cur_reduc=0} = S ) -> pick_new_is( S );
tick_interrupt_steal( running, #state{cur_reduc=_} = S ) -> reduce( S ).

%%% ==========================================================================
%%% Optional ErLam Scheduler API
%%% ==========================================================================

%% @private
%% @doc Returnsthe arguments to pass in to the scheduler as a direct printout.
options() ->
    {ok, 
  "max_reduc - The number of reductions on an expression before pick_next.i\n"++
  "interrupt_steal - Default, but switches sched to use theif processes.\n"++
  "shared_queue - Switches sched to use steal directly from other cores."
    }.

%% @hidden
%% @doc After parsing, update init state with user-changes.
make_state( Options ) ->
    {A,B,C} = erlang:now(), random:seed(A,B,C),
    MaxReducs = proplists:get_value( max_reduc, Options, ?MAX_REDUCS ),
    {ok, QueueID} = erlam_private_queue:start_link(),
    case proplists:get_value( shared_queue, Options, false ) of
        true -> % We chose to use a personal, but shared queue:
            TickFun = fun tick_shared_queue/2,
            {ok, #state{tick_fun=TickFun,
                        procs=QueueID, 
                        max_reduc=MaxReducs}};
        false -> % We chose to use theif process emulation:
            TickFun = fun tick_interrupt_steal/2,
            {ok, #state{tick_fun=TickFun,
                        procs=QueueID,
                        max_reduc=MaxReducs}}
    end.

%%% ==========================================================================
%%% Private functionality
%%% ==========================================================================

%% @hidden
%% @doc Perform a reduction.
reduce( #state{ cur_proc=P, cur_reduc=R, procs=Ps } = State ) ->
    case erlam_rts:safe_step(P) of
        {ok,NP} -> {ok, running, State#state{ cur_proc=NP,
                                                         cur_reduc=R-1 }}; 
        {stop,NP} -> check_on_stop( NP, State );
        {blocked, NPs} -> %% We are swaping, so ask for a new proc, in meantime.
            ok = insert( NPs, Ps ),
            {ok, running, State#state{ cur_proc=nil, cur_reduc=0}};
        {unblocked, [H|T]} ->
            ok = insert( T, Ps ),
            {ok, running, State#state{ cur_proc=H, cur_reduc=R-1 }};
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
insert( [], _ ) -> ok;
insert( [H|T], P ) -> 
    ok = erlam_private_queue:push( H, P ),
    insert(T, P).

%% @hidden
%% @doc Reset a current process and the number of reductions based on user
%%   adjusted max.
%% @end
resetCur( Process, #state{max_reduc=MR} = State ) ->
    State#state{cur_proc=Process, cur_reduc=MR}.

%% @hidden
%% @doc Check private queue for process, if empty, continue to wait, otherwise
%%   start running on it.
%% @end
pick_new_sq( #state{cur_proc=Current, procs=Queue} = State ) ->
    % pop_push( nil, _ ) returns false if queue is empty only.
    case erlam_private_queue:pop_push( Current, Queue ) of
        {ok, Top} -> {ok, running, resetCur( Top, State )};
        false     -> {ok, waiting, State}
    end.

%% @hidden
%% @doc Pick a random LPU and randomly attempt a steal from them by accessing
%%   their private queue directly. (We steal from the bottom of the queue).
%% @end
steal_process_sq( State ) ->
    MyID = erlam_sched:get_id(),
    IDs = erlam_sched:get_ids(),
    LPUID = pick_random_lpu( MyID, IDs ),
    case erlam_private_queue:steal( LPUID ) of
        {ok, Process} -> {ok, running, resetCur( Process, State )};
        false -> {ok, waiting, State}
    end.

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
%% @doc Check for a theif process, if it exists, then run it first. Then
%%  look at private queue to update our current process, however ignore it
%%  if we currently have nothing to do.
%% @end
pick_new_is( #state{cur_proc=nil} = State ) -> pick_new_sq( State );
pick_new_is( State ) -> check_for_theif( State ), pick_new_sq( State ).

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
            NewState = State#state{waiting=false}, 
            {ok, waiting, NewState};
        {ok, {spawn, Process}} ->
            NewState = State#state{waiting=false}, 
            {ok, running, resetCur( Process, NewState )};
        _Other -> {ok, waiting, State}
    end;
steal_process_is( State ) ->
    MyID = erlam_sched:get_id(),
    LPUID = pick_random_lpu( MyID, erlam_sched:get_ids() ),
    erlam_sched:send( LPUID, {steal, MyID}),
    {ok, waiting, State#state{waiting=true}}.

%% @hidden
%% @doc Check if the message queue has a theif message. If it does, then 
%%   perform a possible steal (only fails if queue is empty and ignores
%%   current process).
%% @end
check_for_theif( State ) ->
    case erlam_sched:check_mq() of
        false -> ok; % ignore
        {ok, {spawn, nil}} -> ok; % ignore
        {ok, {spawn, Process}} -> % add process to queue 
            ok = erlam_private_queue:push( Process, State#state.procs );
        {ok, {steal, LPU}} -> % return a process to LPU
            (case erlam_private_queue:pop( State#state.procs ) of
                 false ->
                     erlam_sched:send( LPU, {spawn, nil} );
                 {ok, Top} ->
                     erlam_sched:send( LPU, {spawn, Top} )
            end);
        Other -> ?DEBUG("Other: ~p~n",[Other])
    end.

