%% The ErLam Default Scheduler
%%
%%  This is a simple round-robin FIFO-queue scheduler which demos how to 
%%  implement a scheduler for ErLam. 
%%
%% @author Alexander Dean

-module(erlam_sched_default).
-behaviour(erlam_sched).
-include("debug.hrl").

% Erlam Scheduler Callbacks:
-export([layout/2, init/1, cleanup/1, step/3]).

% Default Scheduler's State:
-record(state,{ processor :: integer(), 
                primary   :: boolean(),
                debugging :: boolean(),
                mq        :: pid(),
                curstep 
              }).

% Internal FIFO Queue
-define(MQ_NAME, erlam_sched_default_mq).
-record(mq_state, { procs = queue:new(), funs = queue:new() } ).

%%% ==========================================================================
%%% ErLam Scheduler Callbacks
%%% ==========================================================================

%% @doc Use the default layout options.
layout( Topology, Options ) -> 
    erlam_sched:layout(?MODULE, Topology, Options).

%% @doc Return the state of the initial 
init( Options ) ->
    Primary = is_primary( Options ),
    MQ = find_or_make_mq( Primary ),
    Processor = get_processor( Options ),
    Debuggery = is_debug( Options ),
    State = #state{
        processor = Processor, 
        primary   = Primary,
        debugging = Debuggery,
        mq        = MQ
    },
    ?DEBUG("INIT: ~p~n---------------~n~p~n=================~n",[Options,State]),
    add_to_queue( Primary, State ),
    {ok, State}.

%% @doc Clean up the state of the system for shutdown 
cleanup( #state{ primary = true, mq = MQ } = State ) ->
    MQState = kill_mq( MQ ),
    clean_state( MQState, State );
cleanup( _ ) -> ok.

%% @doc Step through the system.
step( X, Y, Z ) ->  
    ?DEBUG("STEPPING(~p,~p,~p)~n",[X,Y,Z]),
    {ok, 'WAITING', Y}.
%step( 'WAITING', State, [] ) -> {ok, 'WAITING', State};
%step( 'WAITING', State, [{apply,Fun,Val}|T] ) -> 
%    erlam_rts:reduce( Fun, Val ).
    


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
%% @doc Check options for which processor we're given.
get_processor( Options ) ->
    case lists:keyfind( processor, 1, Options ) of
        {processor, N, _} -> N
    end.

%% @hidden
%% @doc Make the FIFO queue for work stealing if primary, otherwise, just grab
%%   the PID of the registered queue.
%% @end  
find_or_make_mq( false ) -> % Not Primary
    whereis( ?MQ_NAME );
find_or_make_mq( true )  -> % Primary
    Pid = spawn( fun mq_loop/0 ),
    register( ?MQ_NAME, Pid ),
    Pid.

%% @hidden
%% @doc The non-primary schedulers will need to add themselves to the waiting
%%   queue to be included in passing around 
add_to_queue( false, State ) -> advertise_waiting( State );
add_to_queue( true, _ ) -> ok.

%%% ==========================================================================
%%% Message Queue API
%%% ==========================================================================

advertise_waiting( #state{processor=Proc, mq=MQ} ) ->
    ?DEBUG("APPENDING ~p TO MQ~n",[Proc]),
    MQ!{append, Proc},
    ok.

spawn_to_queue( Fun, #state{ mq=MQ } ) ->
    MQ!{spawn, Fun},
    ok.

kill_mq( MQ ) ->
    MQ!{shutdown,self()},
    receive M -> M end.

%%% ==========================================================================
%%% Message Queue Implementation
%%% ==========================================================================

%% @hidden
%% @doc Internal Message Queue Loop which handles forwarding spawned processes
%%   to the correct schedulers.
%% @end  
mq_loop() -> mq_loop( #mq_state{} ).
mq_loop( State ) ->
    receive
        {spawn, Fun} -> handle_spawn( Fun, State );
        {append, Sched} -> handle_append( Sched, State );
        {shutdown, From} -> From!State
    end.

%% @hidden
%% @doc Append the spawned process to the Queue if there are no schedulers 
%%   ready to take it right away. Otherwise send it to the top queued scheduler
%%   and remove it from the waiting queue.
%% @end  
handle_spawn( Fun, #mq_state{ procs = P, funs = F } = State ) ->
    case P of
        [] -> mq_loop( State#mq_state{ funs = (F++[Fun]) } );
        [H|T] -> 
            send_fun_to_sched( Fun, H ),
            mq_loop( State#mq_state{ procs=T } )
    end.

%% @hidden
%% @doc A new scheduler is advertising it's readiness. Send it a waiting 
%%   process if there is one, otherwise queue up the scheduler for any spawned
%%   processes.
%% @end  
handle_append( Sched, #mq_state{ procs = P, funs = F } = State ) ->
    case queue:out( F ) of
        {empty,_} -> 
            mq_loop( State#mq_state{ procs=add_proc(Sched, P) } );
        {{value,H},T} ->
            send_fun_to_sched( H, Sched ),
            mq_loop( State#mq_state{ funs=T } )
    end.
add_proc( P, Ps ) ->
    case queue:member( P, Ps ) of
        true -> Ps;
        false -> queue:in( P, Ps )
    end.

%% @hidden
%% @doc Ship the process to the waiting scheduler. Will get it in the next step
%%   assuming the message queue isn't full on the receiver's end.
%% @end  
send_fun_to_sched( Fun, Sched ) -> 
    erlam_sched:send( Sched, {apply, Fun} ).

%% @hidden
%% @doc Cleans the state of the message queue according to state options.
clean_state( MQState, Options ) -> ok.


