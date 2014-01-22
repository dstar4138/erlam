%%
%% ErLam Runtime System.
%%  This module is archived into the compiled script to be accessed alongside
%% the user's own code and the channel server. This sets everything up and
%% has the most likely chance of changing over time.
%%
-module(erlam_rts).
-export([setup/1,breakdown/0,safe_spawn/1,reduce/2]).

-define(DEFAULT_RTS, orddict:from_list([
                            {verbose, false} % Print RTS Stats?
                                       ])).

%% Start up the channel server and scheduling system.
-spec setup( [{atom(), any()}] ) -> ok | {error, Reason :: any()}.
setup( RTSOptions ) ->
    Options = parse_options( RTSOptions ),
    %% Start the Channel server, will handle swapping and channel creation.
    erlam_chan:start(),
    %% Start the Runtime Monitor, useful for global information gathering.
    erlam_state:start( Options ),
    %% TODO: Start some scheduling system.
    ok.

%% Shutdown channel server, and schedulers.
-spec breakdown() -> ok | {error, Reason :: any()}.
breakdown() ->
    %% Wait for the stopping of all global monitors
    erlam_state:stop(),
    %% Then kill the channels.
    erlam_chan:stop().

%% Message scheduling system with new process.
-spec safe_spawn( fun() ) -> integer().
safe_spawn( Fun ) ->
    erlam_state:inc_processes(),
    erlang:spawn( erlang, apply, [Fun,[0]] ), 1. %TODO: Need to send to scheduling, not erl spawn.

%% Processess a function application through the loaded scheduler.
-spec reduce( fun(), any() ) -> any().
reduce( Fun, Val ) ->
    erlam_state:inc_reductions(),
    erlang:apply( Fun, [Val] ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Parse the command line options passed into the compiled program.
parse_options( [] ) -> ?DEFAULT_RTS;
parse_options( ["-?"|_Rest] ) -> usage(), halt(0);
parse_options( ["-h"|_Rest] ) -> usage(), halt(0);
parse_options( ["-v"|Rest] ) ->
    Dict = parse_options( Rest ),
    orddict:store(verbose, true, Dict);
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
