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
-define(inspect(Process),case Process of 
                             X when is_record(X,process) -> 
                                 erlam_trans:ast2pp(Process#process.exp); 
                             _ -> io_lib:format("~p", [Process])
                         end).

% Erlam Scheduler Callbacks:
-export([layout/2, init/1, cleanup/1, tick/2, spawn_process/2]).

% Default Scheduler's State:
-record(state,{ primary  = false, % Quick check if current sched is primary.
                cur_proc = nil,
                cur_reduc = 0,
                debugging = false
              }).
-define(MAX_STEPS, 20).

%%% ==========================================================================
%%% ErLam Scheduler Callbacks
%%% ==========================================================================

%% @doc Use the default layout options, which is one scheduler per LPU. 
%%   Primary scheduler is on #0.
%% @end
layout( Topology, Options ) -> 
%    [ {0,?MODULE,[{primary,true},{processor,0,3},{debug,true}]}
%    , {1,?MODULE,[{primary,false},{processor,1,3},{debug,true}]}
%    , {2,?MODULE,[{primary,false},{processor,2,3},{debug,true}]}
%    ].
    erlam_scheduler:layout(?MODULE, Topology, Options).

%% @doc Return the state of the initial 
init( Options ) ->
    Primary = is_primary( Options ),
    Debug = is_debug( Options ),
    waiting = find_or_make_mq( Primary ),
    {ok, #state{ primary=Primary,
                 debugging=Debug }}.


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
tick( startup, State ) -> wait_mode( State );
tick( waiting, State ) -> wait_mode( State );
tick( running, #state{cur_reduc=0}=State ) -> get_new_proc( State );
tick( running, #state{cur_reduc=_}=State ) -> reduce( State ).

%% @hidden
%% @doc The mode taken when in startup or waiting mode. We check our mailbox for
%%  work to do.
%% @end
wait_mode( State ) -> 
    case erlam_sched:check_mq() of
        false -> 
            {ok, waiting, State};
        {ok, Msg} ->
            process_mail( Msg, State )
    end.


%%% ==========================================================================
%%% Private Functionality
%%% ==========================================================================

%% @doc Process a message we received on the inter-process channel.
process_mail( {queue_spawn, Process}, State ) ->
    {ok, running, State#state{ cur_proc=Process, cur_reduc=?MAX_STEPS }}.

%% @doc Get a new process, or return to waiting.
get_new_proc( #state{cur_proc=Proc} = State ) ->
    Response = case Proc of 
        nil -> erlam_sched_global_queue:advertise_waiting();
        _   -> erlam_sched_global_queue:peekpush_to_queue( Proc )
    end,
    case Response of
        waiting   -> {ok, waiting, State#state{cur_proc=nil, cur_reduc=0}};
        {ok, New} -> {ok, running, State#state{cur_proc=New,
                                                         cur_reduc=?MAX_STEPS}}
    end.

%% @doc Step the process and reduce our reduction count.
reduce( #state{ cur_proc=P, cur_reduc=R } = State ) ->
    case erlam_rts:safe_step( P ) of
        {ok, NP} -> {ok, running, State#state{cur_proc=NP, cur_reduc=R-1}};
        {stop,NP} -> check_on_stop( NP, State );
        {hang, NP, 0} -> %% We are swaping, so ask for a new proc
            {ok, running, State#state{ cur_proc=NP, cur_reduc=0}};
        {hang, NP, Sleep} -> 
            timer:sleep( Sleep ), % Halt the world style sleep!
            {ok, running, State#state{ cur_proc=NP, cur_reduc=R-1 }};
        {error, Reason} -> exit( Reason )
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


%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Check options whether its the primary scheduler.
is_primary( Options ) ->
    proplists:get_value( primary, Options, false ).

%% @hidden
%% @doc Check options whether we should print debuggery.
is_debug( Options ) ->
    proplists:get_value( debug, Options, false ).

%% @hidden
%% @doc Make the FIFO queue for work stealing if primary, otherwise, just grab
%%   the PID of the registered queue.
%% @end  
find_or_make_mq( false ) -> % Not Primary, so just advertise waiting.
    erlam_sched_global_queue:advertise_waiting();
find_or_make_mq( true )  -> % Primary, so start and link to queue.
    {ok, _} = erlam_sched_global_queue:start_link(),
    waiting.

