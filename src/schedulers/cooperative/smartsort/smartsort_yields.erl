%%
%% Alternative mechanisms for smartsort process yields
%%
-module(smartsort_yields).

-include("smartsort_state.hrl").

-export([default/3]).

%% @doc Default yield implementation. This will simply increase the tracker's
%%   count by one for each process block/unblock. TThis should bring about the 
%%   resort quicker.
%% @end
default( blocked, NPs, #state{procg=Q} = State )->
    lists:foreach( fun(P) -> 
                        bpgraph_serve:inc_count( Q ),
                        bpgraph_serve:push( P, Q ) 
                   end, NPs ),
    {ok, running, State#state{cur_proc=nil, cur_reduc=0}};
default( unblocked, [H|T], #state{procg=Q, cur_reduc=R} = State ) ->
    bpgraph_serve:inc_count( Q ),
    lists:foreach( fun(P) -> 
                        bpgraph_serve:inc_count( Q ),
                        bpgraph_serve:push( P, Q ) 
                   end, T),
    {ok, running, State#state{cur_proc=H, cur_reduc=R-1}}.

%%% TODO: Implement Alternatives:
%%%     There are a couple different methods for yielding here which would be 
%%%     interesting to test. Namely if we ignore the counter on unblock, and only
%%%     update onnblock, we avoid the case of having a bunch of processes
%%%     which communicate once, but still elevate the importance. Thus for every
%%%     completed communication we increase priority.
%%%
