%%
%% Alternative Mechanisms for Process Yields in Longevity Batcher
%%
-module(longbatcher_yields).

-include("debug.hrl").
-include("process.hrl").
-include("longbatcher_state.hrl").

-export([default/3]).


%% @doc When a yield blocks, we return the process to the local batch, if it
%%   does not, we proceed to reduce on it. Note if our channel uses absoption
%%   we may not be able to return the process to the local batch, in which 
%%   case the process which unblocks places it in it's own.
%% @end
default( blocked, NPs, #state{loading_dock=LD}=State ) ->
    NewLD = insert_loading_dock( NPs, LD ),
    NewState =  State#state{ cur_proc=nil, cur_reduc=0 },
    case queue:len( NewLD ) of
        0 -> {ok, running, NewState#state{ loading_dock=nil }}; % Force preempt
        _ -> {ok, running, NewState#state{ loading_dock=NewLD }}
    end;
default( unblocked, [H|T], #state{loading_dock=LD,cur_reduc=R}=State ) ->
    Unmarked = lists:map( fun longbatcher_util:unmarkTid/1, T ),
    NewLD = insert_loading_dock( Unmarked, LD ),
    {ok, running, State#state{ loading_dock=NewLD, cur_proc=H, cur_reduc=R-1 }}.


%% Other Possibilities:
%%  - Processes which are marked when received can be kicked out of the batch.
%%  - Use an incrememnt system to keep track of last communicated and base 
%%      rebatching operation on whether this mark is within a threshold amount
%%      of time since performed.
%%  - ...

%% Notes:
%%  - If channel absorption is not enabled, batches wont be updated based on
%%    cooperation. In fact the best we can do is clear batches based on 
%%    longevity (i.e. if a process is taking too much time away from the
%%    rest, by some metric, we can kick it out).

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Insert a set of processes into the local queue.
insert_loading_dock( L, nil ) -> queue:from_list(L);
insert_loading_dock( [], P )  -> P;
insert_loading_dock( [H|T], P ) -> insert_loading_dock(T, queue:in(H,P)).

