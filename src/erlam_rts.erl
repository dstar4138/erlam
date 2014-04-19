%%
%% ErLam Runtime System.
%%  This module is archived into the compiled script to be accessed alongside
%% the user's own code and the channel server. This sets everything up and
%% has the most likely chance of changing over time.
%%
-module(erlam_rts).
-export([setup/2,breakdown/0,safe_spawn/1]).
-include("debug.hrl").

-define(DEFAULT_SCHED, []).
-define(DEFAULT_STATE,  orddict:from_list([
                            {verbose, false} % Print RTS Stats?
                                       ])).
-define(DEFAULT_RTS,{?DEFAULT_SCHED,?DEFAULT_STATE}).

%% Start up the channel server and scheduling system.
-spec setup( [{atom(), any()}], term() ) -> term().
setup( RTSOptions, Expression ) ->
    {SchedOpts, StateOpts} = parse_options( RTSOptions ),
    %% Start the Channel server, will handle swapping and channel creation.
    erlam_chan:start(),
    %% Start the Runtime Monitor, useful for global information gathering.
    erlam_state:start( StateOpts ),
    %% Intitialize the Schedulers and send the initial expression.
    erlam_sched:run( SchedOpts, Expression ).

%% Shutdown channel server, and schedulers.
-spec breakdown() -> ok | {error, Reason :: any()}.
breakdown() ->
    %% Wait for the stopping of all global monitors
    erlam_state:stop(),
    %% Then kill the channels.
    erlam_chan:stop().

%% Message scheduling system with new process, will return an ErLam integer
%% for success checking.
-spec safe_spawn( fun() ) -> integer().
safe_spawn( Fun ) ->
    case erlam_sched:spawn( Fun ) of
        ok         -> erlam_state:inc_processes(),   1;
        {error, E} -> ?DEBUG("Spawn Error: ~p",[E]), 0
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Parse the command line options passed into the compiled program.
parse_options( [] ) -> ?DEFAULT_RTS;
parse_options( ["-?"|_Rest] ) -> usage(), halt(0);
parse_options( ["-h"|_Rest] ) -> usage(), halt(0);
parse_options( ["-v"|Rest] ) ->
    {Sched, Dict} = parse_options( Rest ),
    {Sched, orddict:store(verbose, true, Dict)};
parse_options( [Unknown|_] ) ->
    io:format("Unknown runtime option: ~s~n",[Unknown]), 
    halt(1).

%% @hidden
%% @doc Print the usage information about the executable.
usage() ->
    EXEC = get_exec(),
    Usage = 
        "usage: "++EXEC++" [options]\n"++
        "Options:\n" ++
        "-? | -h\t This help message.\n" ++
        "-v\t Turn on verbose runtime message.\n",
    io:put_chars( Usage ).

%% @hidden
%% @doc Get the script name for printout.
get_exec() ->
    try escript:script_name() 
    catch _:_ -> "els" 
    end.
