%%% The Private Work-Stealable Queue.
%%%
%%% This server binds to the process which spawns to it and does not leave the
%%% core. See the two methods of interacting with this queue provided by the
%%% multi-core work-stealing round-robin scheduler.
%%%
-module(erlam_private_queue).
-behaviour(gen_server).

-include("debug.hrl").

-export([start_link/0]).
-export([push/2, 
         pop/1,
         pop_push/2,
         steal/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(LOG_QUEUE(LPU,Len),erlam_state:log(LPU,queue_length,Len)).

-define(WORK_STEALING_QUEUES, wsqs).
-record( state, { lpuid=nil, rdyq = queue:new() } ).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the global queue, and link it to the main scheduler.
start_link() ->
    LPUID = erlam_sched:get_id(),
    gen_server:start_link(?MODULE, [ LPUID ], []).

%% @doc Push the current process onto the end of the private queue.
push( Process, Queue ) -> gen_server:cast( Queue, {push, Process} ).

%% @doc Push the current process onto the private queue, only if it's
%%   not empty. If it's not it will get the top of the queue and push the
%%   given to the bottom.
%% @end
pop_push( Process, Queue ) -> gen_server:call( Queue, {pushpop, Process} ).

%% @doc Get the top of the quqeue.
pop( Queue ) -> gen_server:call( Queue, pop ).

%% @doc Send a message to the work stealing queues to steal from a particular
%%   one.
%% @end
steal( LPUID ) ->
    pg:send( ?WORK_STEALING_QUEUES, {steal, LPUID, self()} ),
    receive OK -> OK end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Initialize the private queue by binding it to the local scheduler
%%   and thus its LPU.
%% @end
init( [ ProcID ] ) -> 
     process_flag( scheduler, ProcID ),
     process_flag( sensitive, true   ),
     join_pg( ?WORK_STEALING_QUEUES ),
     {ok, #state{lpuid=ProcID}}.

%% @doc Handle all blocking requests from schedulers. This would include
%%  pushing and poping processes from the queue.
%% @end
handle_call( pop, _From, #state{rdyq=Q,lpuid=LPU}=State ) ->
    case queue:out( Q ) of
        {{value, Proc}, Q2} ->
            ?LOG_QUEUE(LPU,queue:len(Q2)),
            {reply, {ok, Proc}, State#state{rdyq=Q2}};
        {empty, _} ->
            ?LOG_QUEUE(LPU,0),
            {reply, false, State}
    end;
handle_call( {pushpop, Process}, _From, #state{rdyq=Q,lpuid=LPU}=State ) ->
    case queue:out( Q ) of
        {{value, Proc}, Q2} ->
            NewQueue = case Process of nil -> Q2; _ -> queue:in(Process,Q2) end, 
            ?LOG_QUEUE(LPU, queue:len(NewQueue)),
            {reply, {ok, Proc}, State#state{rdyq=NewQueue}};
        {empty, _} ->
            ?LOG_QUEUE(LPU,0),
            (case Process of
                nil -> {reply, false, State};
                _   -> {reply, {ok, Process}, State}
            end)
    end;
handle_call( _Msg, _From, State ) -> {reply, ok, State}.

%% @doc Pushing can be handled asynchronously.
handle_cast( {push, Process}, #state{rdyq=Q,lpuid=LPU}=State ) ->
    ?LOG_QUEUE(LPU,queue:len(Q)+1),
    {noreply, State#state{rdyq=queue:in(Process,Q)}};
handle_cast( _Msg, State ) -> {noreply, State}.

%% @doc Handle steals - which will come from the process group rather than
%%   call or casts.
%% @end
handle_info( {pg_message, _, ?WORK_STEALING_QUEUES, {steal, LPUID, From}},
             #state{lpuid=MyLPU, rdyq=Q} = State ) ->
    case MyLPU == LPUID of
        true -> (case queue:out_r( Q ) of % 
                     {{value, V}, Q2} ->
                         ?LOG_QUEUE(MyLPU, queue:len(Q2)), 
                         From!{ok, V},
                         {noreply, State#state{rdyq=Q2}};
                     {empty,_} ->
                         ?LOG_QUEUE(MyLPU, 0),
                         From!false,
                         {noreply, State}
                 end);
        false -> {noreply, State} %ignore
    end;
handle_info( _Msg, State ) -> 
    {noreply, State}. %ignore other pg_message


%%% The following are default gen_server behavior callbacks and are not used.
terminate( _Reason, _State ) -> ok.
code_change( _OldVSN, State, _Extra ) -> {ok, State}.

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================
%%%     Functions utilized exclusively in this module.

%% @hidden
%% @doc Join a process group. Wraps creation as well (in case it doesn't exist).
join_pg( Group ) ->
    case pg:create( Group ) of
        ok -> pg:join( Group, self() );
        {error, already_created} -> pg:join( Group, self() );
        Err -> Err
    end.


