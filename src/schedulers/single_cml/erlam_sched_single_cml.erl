%%% A Single-Core CML based Dual-Queue Scheduler.
%%%
%%% Utilizes a two level queue and will monitor a process for communication.
%%% In the event of communication, it will mark it as a comunication-bound
%%% process.
%%%
-module(erlam_sched_single_cml).
-behaviour(erlam_scheduler).

%% General Debugery.
-include("debug.hrl").
-include("process.hrl").

%% Erlam Scheduler API
-export([ layout/2, init/1, cleanup/1, tick/2, spawn_process/2 ]).

-define(MAX_REDUCS, 20). % The num of reductions before moving to the next proc
-define(inspect(P), io_lib:format("~s",[erlam_trans:ast2pp(P#process.exp)])).

-record( internal_state, {  cur_proc = nil,
                            cur_reduc = 0,
                            procs = erlam_cml_dualqueue:new() } ).

%%% ==========================================================================
%%% Erlam Scheduler Public API
%%% ==========================================================================

%% @doc Return the Scheduler topology for this scheduler implementation. This
%%   uses only Processor0, and will round robit on the primary work queue.
%% @end
layout( _CPUTop, _Opts ) ->
    ProcID = 0,                % Bind Scheduler to Processor with ID = 0.
    Module = ?MODULE,          % Use the callbacks in this module for scheduling
    Opts = [ {primary,true},   % Set this scheduler as primary (for main function)
             {processor, 0, 1},% Make sure scheduler knows there is only one process
             {debug, true} ],  % Utilize debug settings, which can be turned off above
    [ {ProcID, Module, Opts} ].

%% @doc Initializing the scheduler on the processor is easy as returning default
%%   state. The state will hold the local work queue and thats it.
%% @end
init( _Options ) -> {ok, #internal_state{}}.

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
%%% Private functionality
%%% ==========================================================================

%% @hidden
%% @doc Implements the CML based picker which looks at the dual level queue
%%   to select a process from the primary or secondary queues.
%% @end
pick_next( #internal_state{ cur_proc=C, procs=P } = State ) ->
    ok. %TODO


%% @hidden
%% @doc Perform a reduction.
reduce( #internal_state{ cur_proc=P, cur_reduc=R, procs=_Ps } = State ) ->
    case erlam_rts:safe_step(P) of
        {ok,NP} -> {ok, running, State#internal_state{ cur_proc=NP,
                                                         cur_reduc=R-1 }}; 
        {stop,NP} -> check_on_stop( NP, State );
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

