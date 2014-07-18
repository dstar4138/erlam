%% The ErLam Default Scheduler
%%
%%  This is a simple round-robin FIFO-queue scheduler which utilizes a global
%%  shared queue for N schedulers (where N is the max number of LPU).
%%
%% @author Alexander Dean

-module(erlam_sched_global).
-behaviour(erlam_scheduler).
-include("process.hrl").

% General Debuggery
-include("debug.hrl").

% Erlam Scheduler Callbacks:
-export([layout/2, init/1, cleanup/1, tick/2, spawn_process/2]).
-export([options/0]).

-define(MAX_STEPS, 20).

% Default Scheduler's State:
-record(state,{ primary  = false, % Quick check if current sched is primary.
                cur_proc = nil,
                cur_reduc = 0,
                max_reduc = ?MAX_STEPS 
              }).

%%% ==========================================================================
%%% ErLam Scheduler Callbacks
%%% ==========================================================================

%% @doc Use the default layout options, which is one scheduler per LPU. 
%%   Primary scheduler is on #0.
%% @end
layout( Topology, Options ) -> 
    erlam_scheduler:layout(?MODULE, Topology, Options).

%% @doc Return the state of the initial 
init( Options ) ->
    Primary = erlam_sched:is_primary( ),
    ok = find_or_make_mq( Primary ),
    State = default_state( Options ),
    {ok, State#state{ primary=Primary }}.

%% @doc Clean up the state of the system for shutdown 
cleanup( #state{ primary = true } ) ->
    erlam_sched_global_queue:stop();
cleanup( _ ) -> ok.

%% @doc Pass through the spawn to the global queue. We do not handle this 
%%   differently depending on our LPU location.
%% @end 
spawn_process( Process, State ) ->
    ok = erlam_sched_global_queue:spawn_to_queue( Process ),
    {ok, State}.

%% @doc If we have a process, reduce it until we hit our reduction limit.
%%   If we are waiting, check our message queue to see if the queue sent us
%%   a present!
%% @end
tick( startup, State ) -> get_new_proc( State );
tick( waiting, State ) -> get_new_proc( State );
tick( running, #state{cur_reduc=0}=State ) -> get_new_proc( State );
tick( running, #state{cur_reduc=_}=State ) -> reduce( State ).

%% @private
%% @doc Optional callback to specify that this scheduler takes options.
options() ->
    {ok, 
     "max_reduc - The number of reductions on an expression before pick_next."
    }.


%%% ==========================================================================
%%% Private Functionality
%%% ==========================================================================

%% @hidden 
%% @doc Update the default state with the number of user specified reductions.
default_state( Options ) ->
    Reducs = proplists:get_value( max_reduc, Options, ?MAX_STEPS ),
    #state{ max_reduc=Reducs }.

%% @private
%% @doc Get a new process, or return to waiting.
get_new_proc( #state{cur_proc=Proc} = State ) ->
    Response = case Proc of 
        nil -> erlam_sched_global_queue:ask_for_pop();
        _   -> erlam_sched_global_queue:peekpush_to_queue( Proc )
    end,
    case Response of
        waiting   -> {ok, waiting, State#state{cur_proc=nil, cur_reduc=0}};
        {ok, New} -> {ok, running, State#state{cur_proc=New,
                                               cur_reduc=get_mr(State)}}
    end.

%% @private
%% @doc Step the process and reduce our reduction count.
reduce( #state{ cur_proc=P, cur_reduc=R } = State ) ->
    case erlam_rts:safe_step( P ) of
        {ok, NP} -> {ok, running, State#state{cur_proc=NP, cur_reduc=R-1}};
        {stop,NP} -> check_on_stop( NP, State );
        {blocked, NPs} -> %% We are swaping, so ask for a new proc, in meantime.
            ok = insert(NPs),
            {ok, waiting, State#state{ cur_proc=nil, cur_reduc=0}};
        {unblocked, [H|T]} ->
            ok = insert( T ),
            {ok, running, State#state{ cur_proc=H, cur_reduc=R-1 }};
        {hang, NP} -> %Ignore, but reschedule 
            {ok, running, State#state{ cur_proc=NP, cur_reduc=0 }};
        {error, Reason} -> erlam_sched:error( Reason )
    end.

%% @hidden
%% @doc In the event the process has been computed to it's fullest, check if
%%   it was the primary process, return it, and then stop the scheduler.
%%   Otherwise, we can discard it and keep going!
%% @end  
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


%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Utility Function for getting max_reduc from state.
get_mr( #state{max_reduc=MR} ) -> MR.

%% @hidden
%% @doc Make the FIFO queue for work sharing, only if primary.
%% @end  
find_or_make_mq( false ) -> ok; % Not Primary, so just return.
find_or_make_mq( true )  -> % Primary, so start and link to queue.
    {ok, _} = erlam_sched_global_queue:start_link(), ok.

%% @hidden
%% @doc Insert a set of processes into the global queue.
insert( [] ) -> ok;
insert( [Process|Rest] ) -> 
    ok = erlam_sched_global_queue:spawn_to_queue( Process ),
    insert( Rest ).


