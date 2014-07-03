% The num of reductions before moving to the next proc
-define(MAX_REDUCS, 20).

%%% Internal Scheduler State Object %%%
-record(state, {
% Current Process Status.
 cur_proc = nil, 
 cur_reduc = 0,

% Pinned Channels
 pins = sets:new(),

% Current Scheduler Configuration. 
 procs = nil, % Reference to personal queue
 max_reduc = ?MAX_REDUCS, % User defined max reductions per process execution.

% Scheduler Function References
 preempt_fun = nil, % See chanpin_preempts module for details.
 steal_fun = nil,   % See chanpin_steals module for details.
 spawn_fun = nil,   % See chanpin_spawns module for details.
 yield_fun = nil    % See chanpin_yields module for details.
}).


%%% Process Inspection Utilities %%%
-include("process.hrl").
-define(CHAN_PIN_MARK( C, P ),
        P#process{notes=(case P#process.notes of 
                             undefined -> sets:from_list( [C] );
                             Set -> sets:add_element( C, Set )
                         end)} ).
-define(CHAN_PIN_MATCH( C, P ),
        case P#process.notes of
            undefined -> false;
            Set -> sets:is_element( C, Set )
        end).
