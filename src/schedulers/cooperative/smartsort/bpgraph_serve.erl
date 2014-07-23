%%% 
%%% A Bipartite Graph host server
%%%
%%% This standardizes on the Bipartite graph and erlam_private_queue.
-module(bpgraph_serve).
-behaviour(gen_server).

-include("debug.hrl").

-export([start_link/0, start_link/1]).
-export([push/2, 
         pop/1,
         pop_push/2,
         steal/3,
         did_comm/3,
         inc_count/1,
         get_count/1,
         resort/2,
         popular_sigma/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(LOG_QUEUE(LPU,Len),erlam_state:log(LPU,queue_length,Len)).

-define(WORK_STEALING_QUEUES, wsqs).
-record( state, { lpuid=nil, rdyq = nil, counter=0 } ).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the global queue, and link it to the main scheduler.
start_link() -> start_link( default ).
start_link( SortFun ) ->
    LPUID = erlam_sched:get_id(),
    gen_server:start_link(?MODULE, [ LPUID, SortFun ], []).

%% @doc Push the current process onto the end of the private queue.
push( Process, Queue ) -> gen_server:cast( Queue, {push, Process} ).

%% @doc Push the current process onto the private queue, only if it's
%%   not empty. If it's not it will get the top of the queue and push the
%%   given to the bottom.
%% @end
pop_push( Process, Queue ) ->
    gen_server:call( Queue, {pushpop, Process}, infinity ).

%% @doc Get the top of the quqeue.
pop( Queue ) -> gen_server:call( Queue, pop, infinity ).

%% @doc Send a message to the work stealing queues to steal from a particular
%%   one.
%% @end
steal( LPUID, ChannelID, Threshold ) ->
    pg:send( ?WORK_STEALING_QUEUES, 
             {steal, LPUID, ChannelID, Threshold, self()} ),
    receive 
        false -> false; 
        {ok,_}=OK -> OK 
    end.

%% @doc Add or update some edges in the local graph representation.
did_comm( Processes, ChannelID, Queue ) ->
    gen_server:cast( Queue, {did_comm, ChannelID, Processes} ).

%% @doc Increment the counter responsible for triggering a resort.
inc_count( Queue ) -> gen_server:cast( Queue, inc_count ).

%% @doc Get the number of triggers which happened since the last attempted
%%      resort.
%% @end
get_count( Queue ) -> gen_server:call( Queue, get_count ).

%% @doc Trigger a resort (ONLY IF # of PROCESSES ARE ABOVE THRESHOLD!)
resort( Threshold, Queue ) -> gen_server:call( Queue, {resort, Threshold} ).

%% @doc Get the channels which are popular on the local processing unit.
popular_sigma( Queue ) -> gen_server:call( Queue, popular_sigma ).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Initialize the private queue by binding it to the local scheduler
%%   and thus its LPU.
%% @end
init( [ ProcID, SortFun ] ) -> 
     process_flag( scheduler, ProcID ),
     process_flag( sensitive, true   ),
     join_pg( ?WORK_STEALING_QUEUES ),
     Graph = build_graph( SortFun ),
     {ok, #state{lpuid=ProcID, rdyq=Graph}}.

%% @doc Handle all blocking requests from schedulers. This would include
%%  pushing and poping processes from the queue.
%% @end
handle_call( {pushpop, Process}, _From, #state{rdyq=Q,lpuid=LPU}=State ) ->
    case bpgraph:out( Q ) of
        {{value, Proc}, Q2} ->
            NewQueue = case Process of 
                           nil -> Q2; _ -> bpgraph:in(Process,Q2) 
                       end, 
            ?LOG_QUEUE(LPU, bpgraph:len(NewQueue)),
            {reply, {ok, Proc}, State#state{rdyq=NewQueue}};
        {empty, _} ->
            ?LOG_QUEUE(LPU,0),
            (case Process of
                nil -> {reply, false, State};
                _   -> {reply, {ok, Process}, State}
            end)
    end;
handle_call( get_count, _From, #state{counter=N}=State ) ->
    {reply, {ok, N}, State};
handle_call( pop, _From, #state{rdyq=Q,lpuid=LPU}=State ) ->
    case bpgraph:out( Q ) of
        {{value, Proc}, Q2} ->
            ?LOG_QUEUE(LPU,bpgraph:len(Q2)),
            {reply, {ok, Proc}, State#state{rdyq=Q2}};
        {empty, _} ->
            ?LOG_QUEUE(LPU,0),
            {reply, false, State}
    end;
handle_call( {resort, Threshold}, _From, #state{rdyq=Q}=State ) ->
    case bpgraph:len(Q) >= Threshold of
        true -> NQ = bpgraph:resort( Q ),
                {reply, ok, State#state{rdyq=NQ, counter=0}};
        false -> {reply, ok, State#state{counter=0}}
    end;
handle_call( popular_sigma, _From, #state{rdyq=Q}=State ) ->
    {reply, bpgraph:popular_sigma( Q ), State};
handle_call( _Msg, _From, State ) -> {reply, ok, State}.

%% @doc Pushing can be handled asynchronously.
handle_cast( {push, Process}, #state{rdyq=Q,lpuid=LPU}=State ) ->
    ?LOG_QUEUE(LPU,bpgraph:len(Q)+1),
    {noreply, State#state{rdyq=bpgraph:in(Process,Q)}};
handle_cast( {did_comm, ChannelID, Processes}, State) ->
    {noreply, update_comm( State, ChannelID, Processes ) };
handle_cast( inc_count, #state{counter=N}=State ) ->
    {noreply, State#state{counter=N+1} };
handle_cast( _Msg, State ) -> {noreply, State}.

%% @doc Handle steals - which will come from the process group rather than
%%   call or casts.
%% @end
handle_info( {pg_message, _, ?WORK_STEALING_QUEUES, 
              {steal, LPUID, ChannelID, Threshold, From}},
             #state{lpuid=MyLPU} = State ) ->
    case MyLPU == LPUID of
        true -> perform_steal( From, ChannelID, Threshold, State ); 
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

%% @hidden
%% @doc Build the bpgraph based on the given sort function.
build_graph( SortFun ) ->
    case SortFun of
        default -> bpgraph:new();
        % TODO: Introduce alternative sorting mechanisms.
        % example -> bpgraph:new( fun example/3 );
        _ -> 
            io:format("ERROR: '~p' is an invalid sort function for erlam_sched_coop_ss.",[SortFun]),
            halt(1)
    end.

%% @doc Do a pop_by_sigma/3 form the queue and return the processes.
perform_steal( From, nil, _, #state{rdyq=Q}=S ) -> % No channel specified
    {P, NQ} = bpgraph:out( Q ),
    case P of
        empty -> From!false;
        _ -> From!{ok,[P]}
    end,
    {noreply, S#state{rdyq=NQ}};
perform_steal( From, Sigma, Threshold, #state{rdyq=Q}=S ) ->
    {Rhos, NQ} = bpgraph:pop_by_sigma( Sigma, Threshold, Q ),
    case Rhos of
        [] -> From!false;
        _  -> From!{ok, Rhos}
    end,
    {noreply, S#state{rdyq=NQ}}.

%% @doc Run update_edge for each process->ChannelID.
update_comm( #state{rdyq=Q}=State, ChannelID, Processes ) ->
    DefaultValue = timestamp(), % THAT A COMM HAPPENED, NOT FREQUENCY OR QUANTITY.
    NewQ = lists:foldl( fun( Proc, Qp ) ->
                            bpgraph:update_edge( Proc, ChannelID, DefaultValue, Qp )
                        end, Q, Processes ),
    State#state{rdyq=NewQ}.

%% @hidden
%% @doc Get a timestamp as an integer.
timestamp() -> {_,B,C} = os:timestamp(), ((B*1000)+C).

