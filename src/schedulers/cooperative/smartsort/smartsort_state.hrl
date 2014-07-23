% The num of reductions before moving to the next proc
-define(MAX_REDUCS, 20).

% The default threshold for the number of triggers contributing to a resort.
-define(MAX_COUNT, 15).

% Minimum number of processes to allow for a resort.
-define(MIN_PROC_COUNT, 10).

%%% Internal Scheduler State Object %%%
-record(state, {
% Current Process Status.
 cur_proc = nil, 
 cur_reduc = 0,

% Current Scheduler Configuration. 
 procg = nil, % Reference to personal Graph.
 max_reduc = ?MAX_REDUCS, % User defined max reductions per process execution.

% Sorting Triggering.
 threshold = ?MAX_COUNT, % User defined max trigger for forcing resort.
 min_proc_threshold = ?MIN_PROC_COUNT, % User defined min # of procs to resort.

% Scheduler Function References
 preempt_fun = nil, % See smartsort_preempts module for details.
 steal_fun = nil,   % See smartsort_steals module for details.
 spawn_fun = nil,   % See smartsort_spawns module for details.
 yield_fun = nil    % See smartsort_yields module for details.
}).


%%% Process Inspection Utilities %%%
-include("process.hrl").
