%%
%% Alternative Mechanisms for Spawn in Longevity Batcher
%%
-module(longbatcher_spawns).
-include("longbatcher_state.hrl").

-export([single_batcher/2,push_back_batcher/2]).

%% Update State based on Process and Batch Selection %%
-define(resetCurrent( P, B, S ),
        S#state{ cur_proc=P,                  % Update current process
                 cur_reduc=S#state.max_reduc, % Set epsilon to max again
                 loading_dock=B,              % Update with new batch 
                 rounds=(S#state.rounds+1),   % Inc. rounds to account for new
                 grading=true,                % Tell PREEMPT to break batch
                 waiting=false                % Tell TICK to not steal
               }).

%% @doc If a spawn forces a new batch to be made, push current process
%%  to current batch and push it to queue. A new batch is made for this
%%  new process and it is set as current process.
%% @end
single_batcher( Process, #state{cur_proc=C,loading_dock=B,procs=P}=State ) ->
    UpdatedBatch = queue:in(C, B),
    case queue:len( UpdatedBatch ) >= State#state.max_batch of
        true ->
            NewB = queue:new(),
            erlam_private_queue:push( UpdatedBatch, P ),
            {ok, ?resetCurrent( Process, NewB, State )};
        false ->
            {ok, ?resetCurrent( Process, UpdatedBatch, State )}
    end.


%% @doc If a spawn forces a new batch to be made, push current batch to
%%  queue and keep current process in same batch with new-commer. This
%%  will keep the spawner closer to it's immediate child and allow it
%%  to be quickly tested again for new spawns.
%$ @end
push_back_batcher( Process, #state{cur_proc=C,loading_dock=B,procs=P}=State ) ->
    case queue:len(B) + 2 > State#state.max_batch of
        true ->
            NewB = queue:from_list( [C] ),
            erlam_private_queue:push( B, P ),
            {ok, ?resetCurrent( Process, NewB, State )};
        false ->
            NewB = queue:in( C, B ),
            {ok, ?resetCurrent( Process, NewB, State )}
    end.
    %% Questions: What should become of 'C' if Process is labeled as long 
    %%     running? As currently, it may be forced (perhaps unreasonably)
    %%     into being in a batch by itself.


%% Other possibilities:
%%  - Mark process and then spawn to end of batch
%%  - Ignore batch size max at spawn, only during YIELD.
%%  - Check if current proc is near finishing or not (push to front if not).
%%  - ...

