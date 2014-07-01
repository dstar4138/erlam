%%% A Multi-Core Channel-Pinning Feedback Scheduler
%%%
%%% This scheduler utilizes process cooperativity to attempt optimizions for 
%%% cache locality and parallelism using Channel Pinning to LPUs.
%%% 
-module(erlam_sched_coop_cp).
-behaviour(erlam_scheduler).

%% General Debugery.
-include("debug.hrl").

%% Erlam Scheduler API
-export([ layout/2, init/1, cleanup/1, tick/2, spawn_process/2 ]).
-export([ options/0 ]).

%% Process inspection operations:
-include("chanpin_state.hrl").

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
init( Options ) -> make_state( Options ).

%% @doc Cleanup is simple too, we ignore unfinished processes in the queue.
cleanup( _State ) -> ok.

%% @doc As there are two methods for ticking, this method extracts it from
%%  State and runs it with the given function.
%% @end
%% @see chanpin_preempts
tick( S, T ) -> case catch stick(S,T) of
                    {'EXIT', _}=E -> 
                        ?DEBUG("ERROR: ~p~n",[E]),
                        {error, E};
                    OK -> OK
                end.
stick( startup, #state{preempt_fun=F}=State ) -> F( startup, State );
stick( waiting, #state{steal_fun=F}=State ) -> F( State );
stick( running, #state{cur_reduc=0, preempt_fun=F}=State ) -> F(running, State);
stick( running, State ) -> reduce( State ).

%% @doc Catch the event when we are primary and get first process. 
%%  Alternatively call the user-specified spawn function.
%% @end
%% @see longbatcher_spawns
spawn_process( Process, #state{cur_proc=nil}=State ) ->
    %% The scheduler currently has no process. Either waiting on a steal, 
    %% or in start up mode, or has an empty batch due to process absorption. 
    %% We reset the old batch with this new process.
    {ok, State#state{ cur_proc = Process, 
                      cur_reduc = State#state.max_reduc}};
spawn_process( P, #state{spawn_fun=Spawn}=S ) -> Spawn( P, S ).

%%% ==========================================================================
%%% Optional ErLam Scheduler API
%%% ==========================================================================
%% @private
%% @doc Returnsthe arguments to pass in to the scheduler as a direct printout.
options() ->
    {ok, 
  "max_reduc - The number of reductions on an expression before pick_next.\n"++
  "pushback_batcher - Keep the spawning process with the newly spawned "++
        "process during batch overflow."
    }.

%% @hidden
%% @doc After parsing, update init state with user-changes.
make_state( Options ) ->
    {A,B,C} = erlang:now(), random:seed(A,B,C),
    MaxReducs = proplists:get_value( max_reduc, Options, ?MAX_REDUCS ),
    {ok, QueueID} = erlam_private_queue:start_link(),
    State = build_functionality( Options ),
    {ok, State#state{ procs=QueueID,
                      max_reduc=MaxReducs}}.

build_functionality( _Options ) -> % TODO: Only one currently.
    #state{preempt_fun=fun chanpin_preempts:sq_preempt/2,
           steal_fun=fun chanpin_steals:sq_steal/1,
           spawn_fun=fun chanpin_spawns:default/2,
           yield_fun=fun chanpin_yields:default/3
          }.

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
        {error, Reason} -> exit( Reason ) % Won't handle errors
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

