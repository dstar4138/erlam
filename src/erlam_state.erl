%% 
%% ErLam Runtime Monitor
%%
%%   This module is responsible for keeping track of information about the 
%%   current state of the system which would be global for the schedulers.
%%
-module(erlam_state).
-behaviour(gen_server).

%% API
-export([start/1,stop/0]).
-export([inc_reductions/0, inc_processes/0]).

%% Optimizations
-compile(inline). % Implicit aggressive inlining to this module.

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
            num_reductions = 0, % Number of completed reductions made in total
            num_processes = 1,  % Default is always 1, unless another is spawn
            num_schedulers = 0, %TODO: ... 
            %% Runtime Options
            verbose = false

         }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server.
-spec start( term() ) -> ok.
start( Options ) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []), 
    gen_server:call(?MODULE, test_conn, infinity),
    ok.

%% @doc Stops the server, hangs until completion.
-spec stop() -> ok.
stop() -> 
    try gen_server:call(?MODULE, shutdown,infinity)
    catch _:_ -> ok end, % Intential wait for crash of shutdown call.
    ok.

%% @doc Trigger a reduction increment. Does so non-blockingly.
-spec inc_reductions() -> ok.
inc_reductions() -> gen_server:cast(?MODULE, {update, inc, num_reductions}).

%% @doc Trigger a process increment. Does so non-blockingly.
-spec inc_processes() -> ok.
inc_processes() -> gen_server:cast(?MODULE, {update, inc, num_processes}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init(Args) -> 
    State = parse_args( Args ),
    {ok, State}.

%% @private
%% @doc Handling synch call messages
handle_call(test_conn, _, State) -> 
    {reply, ok, State};
handle_call(shutdown, _, State) ->
    %TODO: Check current state and check if running processes?
    shutdown_inform( State ),
    {stop, normal, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

%% @private
%% @doc Handling async cast messages
handle_cast({update,By,On}, State) ->
    NewState = update_state( By, On, State ),
    {noreply, NewState};
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(_Info, State) -> {noreply, State}.

%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
terminate(_Reason, _State) ->  ok.

%% @private
%% @doc  Convert process state when code is changed. NOT USED.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Inlined, state update based on an operation to a particular variable.
-spec update_state( atom(), atom(), #state{} ) -> #state{}.
update_state( inc, On, State ) -> % Increment
    set_val( On, get_val( On, State ) + 1, State );
update_state( dec, On, State ) -> % Decrement
    set_val( On, get_val( On, State) - 1, State ).

%% @hidden
%% @doc Inlined, will get the value of a variable in the state object.
-spec get_val( atom(), #state{} ) -> term().
get_val( num_reductions, #state{num_reductions=N} ) -> N;
get_val( num_processes,  #state{num_processes=N}  ) -> N;
get_val( num_schedulers, #state{num_schedulers=N} ) -> N;
get_val( verbose,        #state{verbose=V}        ) -> V.

%% @hidden
%% @doc Inlined, will update state based on the variable needing set.
-spec set_val( atom(), term(), #state{} ) -> #state{}.
set_val( num_reductions, V, S ) -> S#state{num_reductions=V};
set_val( num_processes,  V, S ) -> S#state{num_processes=V};
set_val( num_schedulers, V, S ) -> S#state{num_schedulers=V};
set_val( verbose,        V, S ) -> S#state{verbose=V}.

%% @hidden
%% @doc Based on options, can be turned on to be verbose runtime output.
shutdown_inform( State ) ->
    case get_val( verbose, State ) of
        true ->
            io:format( "Num Total Reductions: ~p~n" ++
                       "Num Total Processes: ~p~n"  ++
                       "",[ get_val(num_reductions, State),
                            get_val(num_processes, State) ] );
        false -> ok
    end.

%% @hidden
%% @doc Generates the Monitor's state based on runtime options.
parse_args( Args ) ->
   Default = #state{},
   Verbosity = orddict:fetch( verbose, Args ),
   set_val( verbose, Verbosity, Default ).

