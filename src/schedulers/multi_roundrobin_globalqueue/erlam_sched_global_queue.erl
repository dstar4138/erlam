%%%
%%% ErLam Global Process Queue for the 
%%%         Simple Round-Robin Multi-core Scheduler.
%%%
-module(erlam_sched_global_queue).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([spawn_to_queue/1, 
         ask_for_pop/0,
         peekpush_to_queue/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% The queue will keep track of all processes needing computation. However,
%% in the event there are more processors than processes, a processor's queries
%% will be serialized by the queue's process mailbox automatically. Thus
%% simulating shared state accesses.
-record(state, { procs = queue:new() }).

%% Using Primary LPU==0 for all logging of global queue size.
-define(LOG_QUEUE(Len), erlam_state:log(0,queue_length,Len)).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the global queue, and link it to the main scheduler.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Enqueue a new process to the Global Queue.
spawn_to_queue( Process ) ->
    gen_server:cast( ?MODULE, {spawn, Process} ).

%% @doc Let Queue know the current scheduling processor needs a process to
%%   work on. If there is a process avaliable this will return it. Otherwise
%%   it will return 'waiting'.
%% @end
ask_for_pop() -> gen_server:call( ?MODULE, waiting ).

%% @doc Checks if the queue is empty, in which case it will return the 
%%   passed Process. Otherwise, it will push the process to the queue and
%%   return the top process. All this can be yours in one operation!
peekpush_to_queue( Process ) ->
    gen_server:call( ?MODULE, {peekpush, Process} ).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Handle all blocking requests from schedulers. This would include
%%   process and queue requests.
%% @end 
handle_call( waiting, _From, #state{procs=P}=State ) ->
    case queue:is_empty( P ) of
        true -> %% No waiting processes
            ?LOG_QUEUE(0),
            {reply, waiting, State};
        false ->
            {{value,Proc}, NP} = queue:out( P ),
            ?LOG_QUEUE(queue:len(NP)), 
            NewState = State#state{procs=NP},
            {reply, {ok,Proc}, NewState}
    end;
handle_call( {peekpush, Process}, _From, #state{procs=P}=State ) -> 
    case queue:is_empty( P ) of
        true ->
            ?LOG_QUEUE(0),
            {reply, {ok, Process}, State};
        false ->
            {{value,Top},NP} = queue:out( P ),
            ?LOG_QUEUE(queue:len(NP)+1),
            NewState = State#state{procs=queue:in( Process, NP )},
            {reply, {ok, Top}, NewState}
    end. 

%% @doc Handle all async requests from schedulers. This would include 
%%   process spawns, etc.
%% @end
handle_cast( {spawn, Process}, #state{procs=P}=State ) ->
    ?LOG_QUEUE(queue:len(P)+1),
    {noreply, State#state{procs=queue:in(Process, P)}}.


%%% The following are default gen_server behavior callbacks and are not used.
init([]) -> {ok, #state{}}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

