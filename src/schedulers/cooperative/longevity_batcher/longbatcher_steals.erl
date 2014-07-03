%%
%% Alternative Mechanisms for Work-Stealing in Longevity Batcher.
%%  (Note the close coupling with longbatcher_preempts...)
%%
-module(longbatcher_steals).

-export([shared_queue/1, interrupt_steal/1]).

-include("debug.hrl").
-include("process.hrl").
-include("longbatcher_state.hrl").

-define(resetCur,longbatcher_util:resetCur).

%% @doc Pick a random LPU and randomly attempt a steal from them by accessing
%%   their private queue directly. (We steal from the bottom of the queue).
%% @end
shared_queue( State ) ->
    MyID = erlam_sched:get_id(),
    IDs = erlam_sched:get_ids(),
    LPUID = pick_random_lpu( MyID, IDs ),
    case erlam_private_queue:steal( LPUID ) of
        {ok, ProcessBatch} -> 
            NewState = ?resetCur(ProcessBatch, State ),
            {ok, running, NewState};
        false -> {ok, waiting, State}
    end. 

%% @doc Stealing a process comes in two steps. First if not waiting send a 
%%   random LPU a theif message and set mode to waiting. Next, wait for 
%%   response from LPU when they hit the theif thread.
%% @end
interrupt_steal( #state{ waiting=true } = State ) ->
    case erlam_sched:check_mq( true ) of % Hang scheduler until response.
        {ok, {steal, LPU}} ->
            erlam_sched:send( LPU, {spawn, nil} ),
            {ok, waiting, State};
        {ok, {spawn, nil}} ->
            {ok, waiting, State#state{waiting=false}};
        {ok, {spawn, ProcessBatch}} ->
            NewState = ?resetCur(ProcessBatch,State),
            {ok, running, NewState#state{waiting=false}};
        {ok, {channel_pinning,_}} -> 
            {ok, waiting, State}; % IGNORE
        Other ->
           ?DEBUG("Steal resulted in unknown response: ~p~n",[Other]), 
            {ok, waiting, State}
    end;
interrupt_steal( State ) ->
    MyID = erlam_sched:get_id(),
    LPUID = pick_random_lpu( MyID, erlam_sched:get_ids() ),
    erlam_sched:send( LPUID, {steal, MyID}),
    {ok, waiting, State#state{waiting=true}}.

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

