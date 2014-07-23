%% 
%% Alternative mechanisms for preempting in the smart-sorting scheduler.
%%
-module(smartsort_preempts).

-include("smartsort_state.hrl").

-export([default/2]).

%% @doc Default preemption mechanism checks if it's time to resort and then
%%  chooses the top process from the queue to be the current process. Note that
%%  if it's time to resort, we halt all steal attempts and block process on our
%%  own scheduler too.
%% @end
default( _Status, State ) ->
    check_if_resort( State ),
    pick_next( State ).
    

%%% TODO: Implement Alternatives:
%%% There are some more alternative mechanisms for preemption on the 
%%%   smart sorter scheduler too:
%%%
%%%     - Instead of blocking steals too, have it recognize and return
%%%     a null set so it can try to steal from someone else.
%%%
%%%     - The method for checking if a resort is due could be modified
%%%     in a whole host of ways. The mechanism involved could in theory
%%%     feedback and check whether a resort did any good as well by checking
%%%     the average times each channel was blocked for.
%%%


%% ===========================================================================
%% Internal Implementation
%% ===========================================================================

%% @hidden
%% @doc Check if the queue can be sorted. This will only resort if the number of
%%  triggers since the last resort are above a threshold, and the number of 
%%  processes currently in the queue are above a particular threshold as well.
%% @end
check_if_resort( #state{ trigger_threshold=T,
                         min_proc_threshold=P,
                         procg=G } ) -> bpgraph_serve:resort( T, P, G ).

%% @hidden
%% @doc Pick the next one off the process queue if there is one. If not and we
%%   can't get our old process back, then turn to robbery.
%% @end
pick_next( #state{ cur_proc=P, procg=G, max_reduc=MR } = State ) ->
    case bpgraph_serve:pop_push( P, G ) of
        {ok, NC} -> {ok, running, State#state{cur_proc=NC, cur_reduc=MR}};
        false -> % Force steal by setting cur_proc to nil and status to waiting.
            {ok, waiting, State#state{cur_proc=nil, cur_reduc=0}}
    end.
 
