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

% Default Scheduler's State:
-record(state,{ primary  = false, % Quick check if current sched is primary.
                cur_proc = nil,
                cur_reduc = 0
              }).
-define(MAX_STEPS, 20).

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
    Primary = is_primary( Options ),
    ok = find_or_make_mq( Primary ),
    {ok, #state{ primary=Primary }}.

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

%%% ==========================================================================
%%% Private Functionality
%%% ==========================================================================

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
                                                         cur_reduc=?MAX_STEPS}}
    end.

%% @private
%% @doc Step the process and reduce our reduction count.
reduce( #state{ cur_proc=P, cur_reduc=R } = State ) ->
    case erlam_rts:safe_step( P ) of
        {ok, NP} -> {ok, running, State#state{cur_proc=NP, cur_reduc=R-1}};
        {stop,NP} -> check_on_stop( NP, State );
        {yield, NP} -> %% We are swaping, so ask for a new proc, in meantime.
            {ok, running, State#state{ cur_proc=NP, cur_reduc=0}};
        {hang, NP, Sleep} -> 
            timer:sleep( Sleep ), % Halt the world style sleep!
            {ok, running, State#state{ cur_proc=NP, cur_reduc=R-1 }};
        {error, Reason} -> exit( Reason )
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
%% @doc Check options whether its the primary scheduler.
is_primary( Options ) ->
    proplists:get_value( primary, Options, false ).

%% @hidden
%% @doc Make the FIFO queue for work sharing, only if primary.
%% @end  
find_or_make_mq( false ) -> ok; % Not Primary, so just return.
find_or_make_mq( true )  -> % Primary, so start and link to queue.
    {ok, _} = erlam_sched_global_queue:start_link(), ok.
