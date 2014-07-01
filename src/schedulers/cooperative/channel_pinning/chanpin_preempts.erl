%% 
%% Alternative mechanisms for preemption in channel pinning scheduler
%%
-module(chanpin_preempts).

-export([sq_preempt/2]).

-include("chanpin_state.hrl").

%% @doc Default pick next process without regard to order.
sq_preempt( _, State ) -> pick_next( State ).

%% Alternatives:
%%   - If after so many process spawns/yields we could force a private queue
%%     reordering.

%% ===========================================================================
%% Internal Implementation
%% ===========================================================================

%% @hidden
%% @doc Pick the next one off the process queue if there is one. If not and we
%%   can't get our old process back, then turn to robbery.
%% @end
pick_next( #state{ cur_proc=C, procs=P, max_reduc=MR } = State ) ->
    case erlam_private_queue:pop_push( C, P ) of
        {ok, NC} -> 
            {ok, running, State#state{cur_proc=NC, cur_reduc=MR}};
        false -> 
            {ok, waiting, State#state{cur_proc=nil, cur_reduc=0}} % Force steal
    end.
