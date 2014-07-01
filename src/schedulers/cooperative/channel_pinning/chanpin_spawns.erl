%%
%% Alternative spawnning techniques for the Channel Pinning Scheduler
%%
-module(chanpin_spawns).

-export([default/2]).

-include("chanpin_state.hrl").

%% @doc Simple enqueue of process onto local process queue.
default(Process, #state{procs=Ps} = State) ->
    ok = erlam_private_queue:push( Process, Ps ),
    {ok, State}.


%% Alternatives:
%%  - Enqueue to front or 
%%  - Override current process.
%%
