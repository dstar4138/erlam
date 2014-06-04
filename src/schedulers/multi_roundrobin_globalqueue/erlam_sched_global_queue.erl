%%%
%%% ErLam Global Process Queue for the 
%%%         Simple Round-Robin Multi-core Scheduler.
%%%
-module(erlam_sched_global_queue).
-behaviour(gen_server).

-include("debug.hrl").

%% API
-export([start_link/0]).
-export([spawn_to_queue/1, 
         advertise_waiting/0,
         peekpush_to_queue/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% The queue will keep track of all processes needing computation. However,
%% in the event there are more processors than processes, a processor can
%% advertise it's waiting. At which point the queue will forward spawned
%% processes immediately to the top processor.
-record(state, { procs = queue:new(), % The queue of waiting processes
                 comps = queue:new()  % The queue of waiting processors
               }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the global queue, and link it to the main scheduler.
start_link() ->
    Primary = erlam_sched:get_id(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Primary], []).

%% @doc Enqueue a new process to the Global Queue.
spawn_to_queue( Process ) ->
    gen_server:cast( ?MODULE, {spawn, Process} ).

%% @doc Advertise that the current scheduling processor needs a process to
%%   work on. If there is a process avaliable this will return it. Otherwise
%%   it will be enqueued on the waiting queue.
%% @end
advertise_waiting() ->
    SelfRef = erlam_sched:get_id(),
    gen_server:call( ?MODULE, {waiting, SelfRef} ).

%% @doc Checks if the queue is empty, in which case it will return the 
%%   passed Process. Otherwise, it will push the process to the queue and
%%   return the top process. All this can be yours in one operation!
peekpush_to_queue( Process ) ->
    gen_server:call( ?MODULE, {peekpush, Process} ).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Add the primary process to the initial queue.
init([PrimaryID]) -> 
    State = #state{},
    InitalQueue = State#state.comps,
    {ok, State#state{comps=queue:in(PrimaryID,InitalQueue)}}.


%% @doc Handle all blocking requests from schedulers. This would include
%%   process and queue requests.
%% @end 
handle_call( {waiting, SelfRef}, _From, #state{procs=P, comps=C}=State ) ->
    case queue:is_empty( P ) of
        true -> %% No waiting processes, add Self to comps queue.
            NewState = State#state{comps=queue:in(SelfRef,C)},
            {reply, waiting, NewState};
        false ->
            {{value,Proc}, NP} = queue:out( P ),
            NewState = State#state{procs=NP},
            {reply, {ok,Proc}, NewState}
    end;
handle_call( {peekpush, Process}, _From, #state{procs=P}=State ) -> 
    case queue:is_empty( P ) of
        true ->
            {reply, {ok, Process}, State};
        false ->
            {{value,Top},NP} = queue:out( P ),
            NewState = State#state{procs=queue:in( Process, NP )},
            {reply, {ok, Top}, NewState}
    end. 

%% @doc Handle all async requests from schedulers. This would include 
%%   process spawns, etc.
%% @end
handle_cast( {spawn, Process}, #state{procs=P, comps=C}=State ) ->
    NewState = case queue:is_empty( C ) of
                    true -> %% No waiting computers, push to procs queue
                        State#state{procs=queue:in(Process,P)};
                    false -> %% Pop top of queue, message scheduler with proc.
                        {{value,Comp}, NC} = queue:out( C ),
                        ok = send_process( Process, Comp ),
                        State#state{comps=NC}
               end,
    {noreply, NewState}.

%%% The following are default gen_server behavior callbacks and are not used.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Send a process to a particular Processor denoted by ID for computation.
%%   This assumes the processor is still waiting for a process.
%% @end
send_process( Process, ProcID ) ->
    erlam_sched:send( ProcID, {queue_spawn, Process} ).

