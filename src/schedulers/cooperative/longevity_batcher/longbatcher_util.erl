-module(longbatcher_util).

-include("debug.hrl").
-include("process.hrl").
-include("longbatcher_state.hrl").

-export([resetCur/2, blankOut/1]).
-export([markTid/1,unmarkTid/1,isMarked/1,incrementMark/2]).
-compile(inline). % Ignore function reduction here. These are simple.

%% @doc Mark the Process as Long-Running
markTid( Proc ) -> Proc#process{notes={1,0}}.

%% @doc Mark the Process as Short-Running.
unmarkTid( Proc ) -> Proc#process{notes={0,1}}.

%% @doc Is the process marked as Long-Running.
isMarked( #process{notes={A,B}} ) -> (A>=B);
isMarked( _ ) -> false.

% @doc Incrememnt the number of times it's been marked.
incrementMark( N, #process{notes={A,N}}=Proc ) -> Proc#process{notes={A+1,N}};
incrementMark( N, Proc ) -> Proc#process{notes={1,N}}.

%% @doc Resets the current process in the event we have a new batch. This
%%   also resets the reductions and round counters.
%% @end
resetCur( Batch, #state{ max_reduc=MR } = State ) ->
    {{value,Top},NewBatch} = queue:out(Batch),
    Rounds = queue:len(Batch) * State#state.batch_replay,
    State#state{cur_proc=Top, cur_reduc=MR,
                loading_dock=NewBatch, rounds=Rounds}.

%% @doc Blanks out a state in the event of completely stolen processes or
%%   when a large set of processes stop.
%% @end
blankOut( State ) ->
    State#state{ cur_proc=nil, cur_reduc=0, loading_dock=nil, rounds=0 }.

