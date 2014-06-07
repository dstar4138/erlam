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

-define(MAX_PHASE, 10).
-define(MAX_REDUCS, 20). % The num of reductions before moving to the next proc
-define(inspect(P), io_lib:format("~s",[erlam_trans:ast2pp(P#process.exp)])).

%% Current state keeps track of the current thread and the current reduction,
%% along with the two ready queues. The primary ready queue contains the 
-record( state, { curThread = nil,
                  curReduct = 0,
                  rdyQ1 = queue:new(),
                  rdyQ2 = queue:new()
                } ).

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
init( _Options ) -> {ok, #state{}}.

%% @doc Cleanup is simple too, we ignore unfinised processes in the queue.
cleanup( _State ) -> ok.

%% @doc If we have hit the end of our reductions, pick the next process and 
%%   run one reduction before returning. We can ignore the message queue
%%   as there are no other scheduler's to communicate with.
%% @end  
tick( _Status, #state{ curReduct=R }=State ) ->
    NState = case R of 0 -> pick_next( State ); _ -> State end,
    reduce( NState ).
        
%% @doc Spawn a process. This is called on the local scheduling thread, to 
%%   replace the currently running thread. The current process is enqueued.
%% @end
spawn_process( Process, State ) -> enqueueAndSwitchCurThread( Process, State ).

%%% ==========================================================================
%%% Private functionality
%%% ==========================================================================

%% @hidden
%% @doc Implements the CML based picker which looks at the dual level queue
%%   to select a process from the primary or secondary queues.
%% @end
pick_next( #state{ curThread=T, curReduct=R } = State ) ->
    ok. %TODO


%% @hidden
%% @doc Perform a reduction.
reduce( #state{ curThread=T, curReduct=R } = State ) ->
    case erlam_rts:safe_step(T) of
        {ok,NP} -> {ok, running, State#state{ curThread=NP,
                                                         curReduct=R-1 }}; 
        {stop,NP} -> check_on_stop( NP, State );
        {yield, NP} -> %% This was a swap, and intentional yield
           ok; 
        {hang, NP, Sleep} -> 
        %% We are sleeping, so hang (stop the world style), and set reductions
        %% to zero (ignoring what they were before). Will push the process to
        %% the end of the queue.
            timer:sleep( Sleep ),
            {ok, running, State#state{ curThread=NP, curReduct=0 }};
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
            {ok, running, State#state{ curThread=nil, curReduct=0 }}
    end.

%%% ==========================================================================
%%% Queue Functionality
%%% ==========================================================================
%%%

%% @private
%% @doc Enqueue the process on the primary queue.
enqueue( Process, #state{ rdyQ1=Q1 } = State ) -> 
    { ok, State#state{rdyQ1=queue:in(Process,Q1)}}.

%% @private
%% @doc Enqueue the process and mark it as communication bound.
markAndEnqueue( Process, State ) -> enqueue( markTid(Process), State ).

%% @private
%% @doc Enqueue the old current thread if there is one, and then set this
%%   new one to be the current thread.
%% @end
enqueueAndSwitchCurThread( Process, #state{curThread=T}=State ) ->
    case T of
        nil ->
            setCurThread( Process, State );
        _   -> 
            {ok, NewState} = enqueue( T, State ), % New process takes over
            setCurThread( Process, NewState )
    end.


%% @private
%% @doc Returns the top process in the primary ready queue. It uses the 
%%   secondary queue if there it's primary is empty. This will not set the
%%   returned process as the current thread.
%% @end
dequeue1( #state{ rdyQ1=Q1 } = State ) ->
    case queue:out( Q1 ) of 
        {empty,_} -> dequeue2( State );
        {{value, Top}, NewQ1} -> {ok, Top, State#state{rdyQ1=NewQ1}}
    end.

%% @private
%% @doc Returns the top process of the secondary queue, or false if otherwise.
dequeue2( #state{rdyQ2=Q2} = State ) ->
    case queue:out( Q2 ) of
        {empty, _} -> false;
        {{value, Top}, NewQ2} -> {ok, Top, State#state{rdyQ2=NewQ2}}
    end.

%% @private
%% @doc Move the top process of the secondary queue to the primary.
promote( #state{rdyQ1=Q1, rdyQ2=Q2} = State ) ->
    case queue:out( Q2 ) of
        {empty, _} -> {ok, State};
        {{value, Top}, NewQ2} ->
            {ok, State#state{rdyQ1=queue:in(Top,Q1),rdyQ2=NewQ2}}
    end.


%%% ==========================================================================
%%% Process Functionality
%%% ==========================================================================
%%% Note that we utilize the built in process 'notes' field, which is used to
%%% store scheduler information. We set this to true if it's been marked as a 
%%% communication bound thread.

%% @private
%% @doc Mark the Process as Communication-bound.
markTid( Proc ) -> Proc#process{notes=true}.

%% @private
%% @doc Mark the Process as NOT Communication-bound.
unmarkTid( Proc ) -> Proc#process{notes=false}.

%% @private
%% @doc Is the process marked as Communication bount.
isMarked( #process{notes=N} ) -> (N==true).

%% @private
%% @doc Set the current process. This also updates the number or reductions to
%%   max number.
%% @end
setCurThread( Process, State ) ->
    {ok, State#state{curThread=Process, curReduct=?MAX_REDUCS}}.
% No need for a getCurThread as we can unify over the state during each tick.

