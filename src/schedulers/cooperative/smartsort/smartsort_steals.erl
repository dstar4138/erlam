%%
%% Alternative Mechanisms for Stealing in the Smart Sorting Scheduler.
%%
-module(smartsort_steals).

-include("smartsort_state.hrl").

-export([default/1]).


%% @doc Default stealing mechanism for smartsort scheduler. It will randomly
%%   choose a channel (with a preference for the most popular local one) and
%%   randomly choose another LPU to ask for processes from.
%% @end
default( #state{procg=G} = State ) ->
    Victim = choose_victim(),
    ChoiceChannel = choose_channel( G ),
    case bpgraph_serve:steal( Victim, ChoiceChannel, G ) of
        false -> {ok, waiting, State};
        {ok, Procs} -> 
            ok = add_procs( Procs, G ),
           resetCur( State ) 
    end.

%%% TODO: Implement Alternatives:
%%% Stealing in the smartsort scheduler comes down to three things:
%%%     - Where to steal from back, or generic location
%%%     - What to steal (particular channel based on local usage or particular
%%%       process due to reliance on a large subset of local channels).
%%%     - When to steal (as we want to avoid the costs of possibly removing
%%%       processes from the center of queues, it might be adventagous to 
%%%       stealing ONLY during resorts. (i.e. register your need for work
%%%       on all schedulers, and when they are sorting, they send you work.)
%%%

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Choose a victim LPU for our stealing operation.
choose_victim() ->
    MyID = erlam_sched:get_id(),
    IDs = erlam_sched:get_ids(),
    pick_random_lpu( MyID, IDs ).

%% @hidden
%% @doc Choose a channel from the sigma set to try and steal. This should be one
%%   which has a local high score due to pre-existing processes on the local
%%   queue. If not, it will be of any seen.
%% @end
choose_channel( Graph ) ->
    {ok, List} = bpgraph_serve:popular_sigma( Graph ),
    case List of
        [] -> nil; % Steal any process, don't care about channel.
        [H|T]  ->
             % Steal processes which use channel H, but by some probability
             % mutate our selection to some other random index.
             case random:uniform( 10 ) == 1 of
                 true -> lists:nth( random:uniform( length(T) ), T );
                 false -> H
             end
    end.

%% @hidden
%% @doc Randomly pick a LPU ID from the list of IDs
pick_random_lpu( _, [] ) -> 0;
pick_random_lpu( M, ListOfIDs ) ->
    R = lists:nth(random:uniform(length(ListOfIDs)), ListOfIDs),
    case R == M of
        false -> R; 
        true -> pick_random_lpu( M, ListOfIDs )
    end.

%% @hidden
%% @doc Add all processes in a list to the local private queue.
add_procs( Ps, G ) ->
    lists:foreach( fun(P) -> bpgraph_serve:push( P, G ) end, Ps ).

%% @hidden
%% @doc Try to pop a process off the local queue to set it as current.
resetCur( #state{max_reduc=MR, procg=G} = State ) ->
    case bpgraph_serve:pop( G ) of
        {ok, Proc} -> 
            {ok, running, State#state{cur_proc=Proc, cur_reduc=MR}};
        false ->
            {ok, waiting, State}
    end.
