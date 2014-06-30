% The num of reductions before moving to the next proc
-define(MAX_REDUCS, 20).

% Threshold for when processes get requeued or pushed back.
-define(REDUC_THRESHOLD, 0.1). % (Default 10%)

% Conservative batch size high-end limit. Only checked upon spawns. Could also
% be heuristically decided based on channel usage averages (i.e. the average
% number of unique processes a channel sees could be the max batch size).
-define(BATCH_SIZE_MAX, 20). 

% It may be useful to run through the batch multiple times before asking for a
% new one. The batch replay constant multiplies with the number of processes
% currently in the queue to produce the number of 'rounds' to run before giving
% up the batch.
-define(BATCH_REPLAY, 1).

%%% Internal Scheduler State Object %%%
-record(state, {
% Current Process Status.
 cur_proc = nil, 
 cur_reduc = 0,

% Current Batch information.
 loading_dock = nil, % Current batch being worked on.
 rounds = 0, % Number of rounds (processes) to run before getting a new batch.

% Longevity Marking
 grading = false, % Are we grading the cur_proc for longevity?
 preemption_type = nil, % How we mark a process for longevity.

% Current Scheduler Configuration. 
 procs = nil, % Reference to personal Queue-of-Queues.
 max_reduc = ?MAX_REDUCS, % User defined max reductions per process execution.
 batch_replay = ?BATCH_REPLAY, % User defined constant to find the round count.
 max_batch = ?BATCH_SIZE_MAX, % User defined max batch size.
 waiting = false, % Whether we are currently in thief mode.

% Scheduler Function References
 preempt_fun = nil, % See longbatcher_preempts module for details.
 steal_fun = nil,   % See longbatcher_steals module for details.
 spawn_fun = nil,   % See longbatcher_spawns module for details.
 yield_fun = nil    % See longbatcher_yields module for details.

}).

%%% State logging
-define(PROC(P), case P of nil -> "nil"; 
                           _ ->io_lib:format("P<~p>",[P#process.proc_id])
                 end).
-define(LD(LD), case LD of nil -> "nil";
                           _ -> io_lib:format("Q<~p>",[queue:len(LD)])
                end).
-define(STATE2STR( S ), io_lib:format("{state,(w:~p,g:~p),~s,~p:~p,~s:~p}",
                            [S#state.waiting,
                             S#state.grading,
                             ?PROC(S#state.cur_proc),
                             S#state.cur_reduc,S#state.max_reduc,
                             ?LD(S#state.loading_dock),
                             S#state.rounds])).


