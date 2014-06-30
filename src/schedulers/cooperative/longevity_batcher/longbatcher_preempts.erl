%%
%% Alternative Mechanisms for Process Preemption in Longevity Batcher
%%  (Note the close coupling with longbatcher_steals...)
%%
-module(longbatcher_preempts).

-export([shared_queue/2, interrupt_steal/2]).

-include("debug.hrl").
-include("process.hrl").
-include("longbatcher_state.hrl").

-define(markTid,longbatcher_util:markTid).
-define(resetCur,longbatcher_util:resetCur).
-define(blankOut,longbatcher_util:blankOut).
-define(isMarked,longbatcher_util:isMarked).

%% @doc Preempt on shared queue, which means, as long as our queue is non-empty
%%   we can leave the stealing up to the private queue and just worry about 
%%   how we round-robin. However, we need to mark our current process as long
%%   running if we were in "grading" mode.
%% @end
shared_queue( _, State ) ->
    NewState = perform_mark( State ), % mark process as long-running
    pick_new_process( NewState ).


%% @doc  Tick based on an interruption. At each preemption or yield we'll 
%%   check our message queue for a theif message. In the event of its 
%%   existence we'll respond to their steal.
%% @end
interrupt_steal( startup, State ) -> 
    pick_new_process( State ); 
interrupt_steal( _, State ) -> 
    UpState = check_for_thief( State ),
    NextState = perform_mark( UpState ), % mark process as long-running
    pick_new_process( NextState ).

%%% ==========================================================================
%%% Longevity Marking Implementations
%%% ==========================================================================

%% @private
%% @doc Check the model of preemption for when to mark a process as long
%%   running. Note we provide several options.
%% @end 
perform_mark( #state{cur_proc=nil} = State ) ->
    State; % Bypass
perform_mark( #state{preemption_type=never_mark} = State ) -> 
    State;
perform_mark( #state{preemption_type=mark_if_grading} = State ) ->
    mark_if_grading( State );
perform_mark( #state{preemption_type=always_mark} = State ) ->
    always_mark( State );
perform_mark( #state{preemption_type={mark_after, N}} = State ) ->
    mark_after( N, State ).

%% @private
%% @doc If the current state is in grading mode, mark the current process
%%   as long running and return the new state. We pipe-line this before 
%%   picking a new process from the loading dock. Which means this could
%%   force the process to become a new singleton batch.
%% @end
mark_if_grading( #state{ grading=true, cur_proc=C } = State ) ->
    State#state{cur_proc=?markTid(C), grading=false};
mark_if_grading( State ) -> State.

%% @private
%% @doc If a process has lasted until preemption, mark it. This means it will
%%   ALWAYS be kicked out of a batch if it takes a whole computation cycle.
%% @end
always_mark( #state{cur_proc=C} = State ) -> 
    State#state{cur_proc=?markTid(C)}.

%% @private
%% @doc Mark a process only after it's went through several rounds without
%%   yielding. This gives higher probability the higher N is, but definitely
%%   reduces parallelism.
%% @end
mark_after( N, #state{cur_proc=C} = State ) -> 
    State#state{ cur_proc=longbatcher_util:incrememntMark(N, C) }.

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Loads a new process into the cur_proc slot based on the loading dock
%%   selection first, otherwise it will check it's own queue for a new batch
%%   and then set itself as waiting so it can steal.
%% @end
pick_new_process( #state{loading_dock=nil,procs=Queue} = State ) ->
    case erlam_private_queue:pop( Queue ) of
        {ok, ProcessBatch} -> {ok, running, ?resetCur( ProcessBatch, State )};
        false -> {ok, waiting, State}
    end;
pick_new_process( #state{cur_proc=C,procs=Ps,loading_dock=LD,rounds=0} = State ) ->
    case enqueue_then_pop_push( C, LD, Ps ) of
        {ok, ProcessBatch} ->
            {ok, running, ?resetCur( ProcessBatch, State )};
        false -> {ok, waiting, ?blankOut(State)}
    end;
pick_new_process( #state{cur_proc=nil,loading_dock=LD,rounds=R,procs=Ps}=State ) ->
    case queue:len( LD ) of
        0 -> 
            (case erlam_private_queue:pop( Ps ) of
                 false -> {ok, waiting, ?blankOut(State)};
                 {ok,NewLD} -> {ok, running, ?resetCur( NewLD, State )}
             end);
        _ -> 
            NewState = ?resetCur( LD, State ),
            {ok, running, NewState#state{rounds=noNegative(R-1)}}
    end;
pick_new_process( #state{cur_proc=C,loading_dock=LD,rounds=R,procs=P}=State ) ->
    Batch = case {?isMarked(C),queue:len(LD)} of 
                {true, 0} -> queue:in( C, LD );
                {true,_} -> % Push the Long-running process as a new batch
                    Singleton = queue:from_list([C]),
                    ok = erlam_private_queue:push(Singleton,P),
                    LD; % Return current Batch as new Batch.
                {false, _} -> queue:in( C, LD )
            end,
    NewState = ?resetCur( Batch, State ),
    {ok, running, NewState#state{ rounds=noNegative(R-1) }}.

%% @hidden
%% @doc Pushes any value below zero back up to zero.
noNegative( N ) when N < 0 -> 0;
noNegative( N ) -> N.

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
    Batch = case {?isMarked( CurProc ),queue:len(LD)} of
                {true, 0} -> queue:in( CurProc, LD );
                {true, _} ->
                    Singleton = queue:from_list([CurProc]),
                    ok = erlam_private_queue:push( Singleton, Ps ),
                    LD;
                {false, _} -> queue:in( CurProc,LD )
            end,
    erlam_private_queue:pop_push( Batch, Ps ).


%% @hidden
%% @doc Check if the message queue has a theif message. If it does, then 
%%   perform a possible steal (only fails if queue is empty and ignores
%%   current process).
%% @end
check_for_thief( State ) ->
    case erlam_sched:check_mq( false ) of % Override messaging
        false -> State; % ignore
        {ok, {spawn, nil}} -> State; % ignore
        {ok, {spawn, Process}} -> % add process to queue 
            ?resetCur( Process, State );
        {ok, {steal, LPU}} -> % return a process batch to LPU
            (case erlam_private_queue:pop( State#state.procs ) of
                 false -> erlam_sched:send( LPU, {spawn, nil} );
                 {ok, Top} -> erlam_sched:send( LPU, {spawn, Top} )
            end), 
            State;
        Other -> 
            ?DEBUG("Other: ~p~n",[Other]), 
            State
    end.

