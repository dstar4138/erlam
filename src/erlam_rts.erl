%%
%% ErLam Runtime System.
%%  This module is archived into the compiled script to be accessed alongside
%% the user's own code and the channel server. This sets everything up and
%% has the most likely chance of changing over time.
%%
-module(erlam_rts).
-export([setup/1,breakdown/0,safe_spawn/1]).


%% Start up the channel server and scheduling system.
-spec setup( [{atom(), any()}] ) -> ok | {error, Reason :: any()}.
setup( _ ) ->
    erlam_chan:start(). %TODO: Obviously theres more to do, but this will do for testing.

%% Shutdown channel server, and schedulers.
-spec breakdown() -> ok | {error, Reason :: any()}.
breakdown() ->
    erlam_chan:stop().

%% Message scheduling system with new process.
-spec safe_spawn( fun() ) -> integer().
safe_spawn( Fun ) -> 
    erlang:spawn( erlang, apply, [Fun,[0]] ), 1. %TODO: Need to send to scheduling, not erl spawn.
