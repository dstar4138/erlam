%% An Erlam Single Core Scheduler.
%%
%%  This is an example of a single core round-robin scheduler. It can be 
%%  modified to use a different default number of reductions before jumping
%%  to the next process.
%%

-module(erlam_sched_single).
-behavior(erlam_scheduler).

%% General Debugery.
-include("debug.hrl").
-include("process.hrl").

%% Erlam Scheduler API
-export([ layout/2, init/1, cleanup/1, tick/2, spawn_process/2 ]).
-export([ options/0 ]).

-define(MAX_REDUCS, 20). % The num of reductions before moving to the next proc
-define(inspect(P), io_lib:format("~s",[erlam_trans:ast2pp(P#process.exp)])).

-record( internal_state, {  cur_proc = nil,
                            cur_reduc = 0,
                            max_reduc = ?MAX_REDUCS,
                            procs = queue:new() } ).

%%% ==========================================================================
%%% Erlam Scheduler Public API
%%% ==========================================================================

%% @doc Return the Scheduler topology for this scheduler implementation. This
%%   uses only Processor0, and will round robit on an internal work queue.
%% @end
layout( _CPUTop, Options ) ->
    ProcID = 0,                % Bind Scheduler to Processor with ID = 0.
    Module = ?MODULE,          % Use the callbacks in this module for scheduling
    Opts = parse_sched_opts( Options ), % Pull out options passed to scheduler
    { ProcID,                  % Set this scheduler as primary (for main function)
       [ {ProcID, Module, Opts} ]}.

%% @doc Initializing the scheduler on the processor is easy as returning default
%%   state. The state will hold the local work queue and thats it.
%% @end
init( Options ) -> get_options( Options, #internal_state{} ).

%% @doc Cleanup is simple too, we ignore unfinised processes in the queue.
cleanup( _State ) -> ok.

%% @doc If we have hit the end of our reductions, pick the next process and 
%%   run one reduction before returning. We can ignore the message queue
%%   as there are no other scheduler's to communicate with.
%% @end  
tick( _Status, #internal_state{ cur_reduc=R }=State ) ->
    NState = case R of 0 -> pick_next( State ); _ -> State end,
    reduce( NState ).
        
%% @doc Spawn a process. This is called on the local scheduling thread,
%%   but for this implemenation we don't care, as long as it's added to the
%%   state.
%% @end
spawn_process( Process, #internal_state{procs=P} = State ) ->
    {ok, State#internal_state{procs=queue:in( Process, P )}}.


%%% ==========================================================================
%%% Optional ErLam Scheduler API
%%% ==========================================================================

%% @private
%% @doc Returnsthe arguments to pass in to the scheduler as a direct printout.
options() ->
    {ok, 
     "max_reduc - The number of reductions on an expression before pick_next."
    }.

%% @hidden
%% @doc Parse the options which are passed to the scheduler. You can use any
%%   style parameters you want. But the default schedulers use the style 
%%   described by erlam_scheduler:default_parse_opts/1.
%% @end
parse_sched_opts( Options ) ->  erlam_scheduler:default_parse_opts( Options ).    

%% @hidden
%% @doc After parsing, update init state with user-changes.
get_options( Options, State ) ->
    Reducs = proplists:get_value( max_reduc, Options, ?MAX_REDUCS ),
    {ok, State#internal_state{max_reduc=Reducs}}.


%%% ==========================================================================
%%% Private functionality
%%% ==========================================================================

%% @hidden
%% @doc Implements the fair round-robin selection from the 'procs' queue.
pick_next( #internal_state{ cur_proc=C, procs=P } = State ) ->
    {Selection, Queue} =
        case queue:out( P ) of
            {empty, _} -> {C,P}; %% Only one process, so repeat.
            {{value,V},Q} -> {V,Q}
        end,
    NewQueue = case C of nil -> Queue; _ -> queue:in(C, Queue) end,
    State#internal_state{ cur_proc = Selection,
                          cur_reduc = get_mr(State),
                          procs = NewQueue }.

%% @hidden
%% @doc Perform a reduction.
reduce( #internal_state{ cur_proc=P, cur_reduc=R, procs=_Ps } = State ) ->
    case erlam_rts:safe_step(P) of
        {ok,NP} -> {ok, running, State#internal_state{ cur_proc=NP,
                                                         cur_reduc=R-1 }}; 
        {stop,NP} -> check_on_stop( NP, State );
        {yield, NP} -> 
            {ok, running, State#internal_state{cur_proc=NP, cur_reduc=0}};
        {hang, NP, Sleep} -> 
        %% We are sleeping, so hang (stop the world style), and set reductions
        %% to zero (ignoring what they were before). Will push the process to
        %% the end of the queue.
            timer:sleep( Sleep ),
            {ok, running, State#internal_state{ cur_proc=NP, cur_reduc=0 }};
        {error, Reason} -> exit( Reason ) % Won't handle errors
    end.
check_on_stop( Process, State ) ->
    case ?is_primary( Process ) of
        %% If it finished and was the primary process, return it!
        true -> 
            erlam_sched:return( Process ), %will handle unwraping
            {stop, State};
        %% If it finished and was not primary, then drop it and move to next
        false -> 
            {ok, running, State#internal_state{ cur_proc=nil, cur_reduc=0 }}
    end.

%% @hidden
%% @doc Utility Function for getting max_reduc from state.
get_mr( #internal_state{max_reduc=MR} ) -> MR.

