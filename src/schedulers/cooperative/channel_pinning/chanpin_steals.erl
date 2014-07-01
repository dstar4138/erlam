%%
%% Alternative Mechanisms for SharedQueue Work Stealing Channel Pinner.
%%
-module(chanpin_steals).

%% API
-export([sq_steal/1]).

-include("chanpin_state.hrl").

%% We are not allowed to steal over 40% of another's queue.
-define(THRESHOLD, 0.4).

sq_steal( #state{procs=Ps, pins=MyChans} = State ) ->
    MyID = erlam_sched:get_id(),
    IDs = erlam_sched:get_ids(),
    LPUID = pick_random_lpu( MyID, IDs ),
    Op = default( MyChans ),
    case erlam_private_queue:steal_by_op( LPUID , Op ) of
        {ok, Procs} ->
            ok = add_list( Procs, Ps ),
            resetCur( State );
        false -> {ok, waiting, State}
    end.

%%% ==========================================================================
%%% Stealing Operations
%%% ==========================================================================
%% Steal by Ops must return one of three values:
%%  - true = would like to steal
%%  - false = would not like
%%  - stop = give me what i've selected
%% and their parameters are: (TotalProcCount, CountSoFar, CurrentProc)

default( MyChans ) ->
   fun( Total, Size, Cur ) ->
        case (Size / Total) > ?THRESHOLD of
           true -> stop;
           false -> 
                Procs = case Cur#process.notes of 
                            undefined -> sets:new();
                            Set -> Set
                        end,
                Intersection = sets:intersection( [ MyChans, Procs ] ),
                (sets:size(Intersection) >= 1)
        end
    end.
                

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

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
add_list( L, P ) ->
    lists:foreach( fun(X) -> erlam_private_queue:push( X, P ) end, L ).

%% @hidden
%% @doc Try to pop a process off the local queue to set it as current.
resetCur( #state{max_reduc=MR, procs=Ps} = State ) ->
    case erlam_private_queue:pop( Ps ) of
        {ok, Proc} -> 
            {ok, running, State#state{cur_proc=Proc, cur_reduc=MR}};
        false ->
            {ok, waiting, State}
    end.
