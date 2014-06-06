%%% Erlam Scheduler Behaviour and Default Callback Implementation
%%%
%%%  
-module( erlam_scheduler ).
-include( "process.hrl" ).
-include( "erlam_scheduler.hrl" ).

%% Default implementations
-export( [ layout/3 ] ).

%% Private
-export( [ required/0 ] ).

%%% ==========================================================================
%%% Callback registration for Scheduler Behaviours
%%% ==========================================================================

%% Before SETUP when we are defining how many schedulers to run, we perform
%% a scan of the hardware we are running on and then consult the desired
%% scheduler for how to utilize the topology effectively. Thus we pass in the
%% topology and expect a layout map in return. 
-callback layout( erlang:cpu_topology(), scheduler_opts() ) -> 
                                                            scheduler_layout().


%% During SETUP phase, for each logical processor, a new scheduler process will
%% be constructed. This will return the state of the current scheduler. An 
%% example of the state could be a PID for a shared queue that was started by
%% the main scheduler.
-callback init( scheduler_opts() ) -> {ok, scheduler_state()}
                                    | {error, log_msg()}
                                    | {warn, log_msg(), scheduler_state()}.


%% During BREAKDOWN phase, for each logical processor, we call them in reverse
%% order so that the primary processor is last. This should clean up the state.
-callback cleanup( scheduler_state() ) -> ok
                                        | {error, log_msg()}
                                        | {warn, log_msg()}.


%% A scheduler is a FSM. In classical scheduling we can be in a WAITING, 
%% RUNNING, or BLOCKED status. So to be general for each simulated 'tick' we 
%% call step at theoretically the same time on all schedulers.
-callback tick( scheduler_status(), scheduler_state() ) -> 
             {ok, scheduler_status(), scheduler_state()} |
             {return, term()}                            |
             {stop, scheduler_state()}                   . 


%% When the RTS evaluates the spawn command, it will patch into the primary
%% scheduler module (not the physical process) and call this function. It's
%% goal is to add the process to whatever queue it needs to.
-callback spawn_process( erlam_process(), scheduler_state() ) -> 
                                       {ok, scheduler_state()} |
                                       {error, log_msg()}      .

%%% ==========================================================================
%%% Default Callback implementations
%%% ==========================================================================

%% @doc Generates a default scheduler layout with one module per logical
%%   processing unit. This disregards exact topology and just returns
%%   the max list of modules. Note: schedulers can just call this function
%%   from their layout/2 function if they want to. 
%% @end
layout( Module, Topology, Options ) ->
    % Count the number of LPU's on each branch of the Topology. 
    Count = lists:foldl( fun(X,C)-> 
                                 count_logical(X,0)+C 
                         end, 0, Topology ),
    % With the count, generate a list of sched_desc() for each LPU.
    % If LPU is 0, then it will set it as primary.
    lists:map( fun(N)-> 
                    {N,Module,option_update(N, Count, Options)} 
               end, lists:seq(0, Count-1)).


%%% ==========================================================================
%%% Private Functionality
%%% ==========================================================================

%% @private
%% @doc The required functionality and their arity. The RTS checks the loaded
%%   scheduler module to verify it has what it needs.
%% @end
required() ->
    [ {layout, 2}, {init,1}, {cleanup, 1}, {tick, 2}, {spawn_process,2} ].

%% @hidden
%% @doc Count the number of Logical Processing units in a cpu_topology().
count_logical({logical,_},N)-> N+1;
count_logical({thread,L},N) -> recurse_logic(L,N);
count_logical({core,L},N)   -> recurse_logic(L,N);
count_logical({processor,L},N) -> recurse_logic(L,N).
recurse_logic(L,N) when is_list(L) -> 
    lists:foldl(fun(X,M)-> count_logical(X,M) end, N, L);
recurse_logic(X,N) -> count_logical(X,N).

%% @hidden
%% @doc Update user options in topology generation.
option_update(N, Max, Options)-> 
    Update = fun (Tuple, List) ->
                lists:keystore( element(1,Tuple), 1, List, Tuple )
             end,
    DefaultOptions = [{primary,(N==0)}, {processor,N,Max}], 
    lists:foldl( Update,  Options, DefaultOptions ).

