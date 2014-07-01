%%
%% Alternative Mechanisms for process yields in channel pinning.
%%
-module(chanpin_yields).

-export([default/3]).

-include("chanpin_state.hrl").

%% @doc Default Channel pinning yield operation. It merely marks the process
%%   as having communicated on the particular channel 
%% @end
default( blocked, NPs, #state{procs=Ps} = State ) ->
    lists:foreach( fun(P) -> erlam_private_queue:push( P, Ps ) end, NPs ), 
    {ok, running, State#state{cur_proc=nil, cur_reduc=0}};
default( unblocked, NPs, #state{procs=Ps, cur_reduc=R}=State ) ->
    [H|T] = mark_by_channel( NPs ),
    lists:foreach( fun(P) -> erlam_private_queue:push( P, Ps ) end, T ), 
    {ok, running, State#state{cur_proc=H, cur_reduc=R-1}}.


%% Alternatives:
%%  - Insteal of marking just for confirmation:
%%      - Keep track of frequency
%%      - Order or at least last used



%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Mark the channel by copying it's most recent channel ID into the 
%%  scheduler-notes field for later review during stealing.
%% @end
mark_by_channel( [] ) -> [];
mark_by_channel( [#process{chan_id=C}=H|T] ) ->
    [ ?CHAN_PIN_MARK( C, H ) | mark_by_channel(T) ].

