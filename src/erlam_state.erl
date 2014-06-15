%%% 
%%% ErLam Runtime Monitor
%%%
%%%     This module is responsible for keeping track of information about the 
%%%     current state of the system which would be global for the schedulers.
%%%     Allows for the logging of miscellaneous timestamped messages in either
%%%     quantity or frequency based CSV files. A Quantity based log looks like
%%%     a heatmap in a LPU to time (grouped in a variable size time-range). 
%%%     This helps to visualize statistics like size of process queue over time.
%%%     The Frequency based log is a linear distribution plot (Gantt) of type 
%%%     to time. This helps to visualize statistics like state of the scheduler 
%%%     over time (i.e. waiting or running).
%%%
-module(erlam_state).
-behaviour(gen_server).

-include("debug.hrl").

%% API
-export([start/1,stop/0]).
-export([note/1,log/2,log/3]).

%% Optimizations
-compile(inline). % Implicit aggressive inlining to this module.

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, { fd = nil,
                 verbose = false %% Runtime Options
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

%% @doc Note that an event happened on the calling LPU. Will not block.
-spec note( term() ) -> ok.
note( Event ) -> log( erlam_sched:get_id(), Event ).

%% @doc Report that something happened at a particular timestamp. For example
%%   log(2,reduce) or log(2, spawn) to increment the counts of either reductions
%%   or spawns. These are useful for keeping track of events in time with others
%% @end
log(LPU, Report) -> 
    gen_server:cast(?MODULE, {log, LPU, Report, os:timestamp()}).

%% @doc Report that not only something happened but the value of it's return, 
%%   namely if it's something we're measuring like process queue size or number
%%   of open channels over time.
%% @end
log(LPU, Name, CurValue) -> 
    gen_server:cast(?MODULE, {log, LPU, Name,CurValue, os:timestamp()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc initilize the log file if need be.
init(Args) -> {ok, parse_args(Args)}.

%% @doc handle shutdown based on State
handle_call(shutdown, _, State) -> 
    shutdown_inform( State ), {stop, normal, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

%% @doc Handle logging based on state.
handle_cast(Log, State) -> emit(Log,State), {noreply,State}.

%% Unused callbacks, the following are default implementations.
terminate(_Reason, _State) -> ok.
handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc The Header written to the CSV file for later parsing by R.
header() -> io_lib:format("timestamp,lpu,event,value~n",[]).

%% @hidden
%% @doc Append a log entry into a file. 
emit(_, #state{verbose=false}) -> ok; %Skip if we aren't logging.
emit({log, LPU, Name, {_,S,M}}, #state{fd=F}) ->
   file:write(F,io_lib:format("~6..0B.~6..0B,~p,~p,\"\"~n",[S,M,LPU,Name])); 
emit({log, LPU, Name, Val, {_,S,M}}, #state{fd=F}) ->
   file:write(F,io_lib:format("~6..0B.~6..0B,~p,~p,~p~n",[S,M,LPU,Name,Val])). 

%% @hidden
%% @doc Based on options, can be turned on to be verbose runtime output.
shutdown_inform( #state{fd=nil} ) -> ok;
shutdown_inform( #state{fd=F} )   -> file:close( F ).

%% @hidden
%% @doc Generates the Monitor's state based on runtime options.
parse_args( Args ) ->
    Verb = orddict:fetch( verbose, Args ),
    #state{ fd = load_file( Verb, Args ),
            verbose = Verb}.

load_file( false, _ ) -> nil;
load_file( _, Args ) ->
    Name = makeName( Args ),
    case file:open( Name, [append] ) of
        {ok, FD} ->
            ok = file:write( FD, header() ),
            FD;
        {error, Reason} ->
            io:format("ERROR (~p): Could not create log file: ~p~n",
                      [Reason, Name]),
            halt(1)
    end.

makeName( _Args ) -> 
    {ok, CWD} = file:get_cwd(),
    CWD++"/test.erlamlog". %TODO: get name from args

