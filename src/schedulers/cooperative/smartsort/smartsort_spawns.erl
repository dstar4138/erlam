%%
%% Alternative mechanisms for spawning processes in the Smart-Sorting Scheduler
%%
-module(smartsort_spawns).

-include("smartsort_state.hrl").

-export([default/2]).


%% @doc Default spawn is to just push the process onto the 
%%   process queue, and increase the counter which 
%% @end
default( Process, #state{procg=Q} = State ) ->
    bpgraph_serve:push( Process, Q ),
    bpgraph_serve:inc_count( Q ),
    {ok, State}.

%%% TODO: Implement Alternatives:
%%% There are plenty of oportunities to expand on the spawn operation:
%%%
%%%     - Add a temporary/weak link between all channels the spawning
%%%       parent process has been in contact with. This includes ones
%%%       it has created (would capture possible future communication).
%%%
%%%     - Ignore incrementing the resort, and just place the new process
%%%       directly after the parent process (or replace it). This would
%%%       keep it close by it's parent and potential partner in a sync.
%%%
%%%     - Spawn to a secondary queue that is only accessed after 
%%%       exahsting primary. When a resort is triggered, that's when 
%%%       the secondary and primary are merged. This is similar to 
%%%       some Multi Level Feedback Queues which escilate all priority
%%%       levels after some time.
%%%
