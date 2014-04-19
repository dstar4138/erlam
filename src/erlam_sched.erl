%% The ErLam Process Scheduler
%%
%%  The Erlam scheduler is actually N different processes where N is the number
%%  of threads (if hyperthreaded) or processors (if otherwise). In otherwords,
%%  ErLam creates the maximum number of processes possible so as to force a 
%%  scheduler per logical processing unit. These processes are then bound to
%%  that particular processor.
%%
%%  Note: We start the main scheduler first and wait for a successful return
%%    before starting any of the other processor schedulers. This is to allow
%%    for external process creation for shared queues, etc. The main scheduler
%%    is determined by the order of logical processors, in that the first is
%%    primary. See erlam_sched_sup module for more information.
%%
%% @author Alexander Dean
-module(erlam_sched).
-include("debug.hrl").

%PUBLIC
-export([run/2, spawn/1, start_link/3, stop/1, stopall/0, broadcast/1, send/2]).

%PUBLIC - DEFAULTS
-export([layout/3]).

%PRIVATE
-export([init/4, sched_opts/0, init_ack/0]).

%%% ==========================================================================
%%% Scheduler Type Checking
%%% ==========================================================================

%%% The Scheduler can return a state which will be weaved through.
-type scheduler_state() :: any().

%%% The inter-process message queue which can be modified in-between steps.
-type message_queue() :: list( any() ).

%%% ErLam can auto handle warnings and errors based on runtime verbosity.
-type log_msg() :: string() | atom() | iodata().

%%% The processor ID that the scheduler is bound to.
-type proc_id() :: unbound | non_neg_integer().

%%% The layout perscribed to the particular CPU topology of the system by 
%%% this particular scheduler module. Thus we could have multiple layouts based
%%% on options and hardware layouts.
-type scheduler_layout() :: term(). %TODO: properly define. 
                            
%%% Options that are passed into the scheduler at runtime are as follows:
-type scheduler_opt() :: 
    % The processor binding out of the total number of processors.
    {processor, proc_id()}             |
    {total_procs, non_neg_integer()}   |
    % The processor which will be considered primary (start the computation).
    {primary, proc_id()}               |
    % Whether it's a debug run.
    {debug, boolean()}                 |
    % How long between steps will the scheduler hang if no messages recieved.
    {message_hang, integer()}          |
    % Max number of messages to add to the buffer between steps. -1 is all.
    {message_buffer, integer()}.
    %TODO: We also need options for throttling statistics gathering.
-type scheduler_opts() :: list( scheduler_opt() ).

-define(SCHED_OPTS, [{processor,3}, % {_, 2, 8} - ID of 8 logical 
                     {primary,2},   % {_, false} - or true if so.
                     {debug,2,optional},         % {_, BOOL}
                     {message_hang,2,optional},  % {_,X} when X is in sec.
                     {message_buffer,2,optional} % {_,X} when X is a count.
                    ]).
-define(DEFAULT_MESSAGE_HANG,    1). % Default is break after waiting a second.
-define(DEFAULT_MESSAGE_BUFFER, -1). % Default is get everything

-type scheduler_status() :: 'WAITING' | 'RUNNING' | 'BLOCKED'.


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
-callback step( scheduler_status(), scheduler_state(), message_queue() ) -> 
                    {ok, scheduler_status(), scheduler_state()} |
                    {stop, scheduler_state()}. 


%%% ==========================================================================
%%% Public API
%%% ==========================================================================

-define(SCHEDULER_GROUP, erlam_sched).

%% @doc Send the initial expression to the primary processor and then waits
%%   for the result message. This function will "run" an expression using the
%%   scheduling options and nothing more.
%% @end  
run( SchedOpts, Expression ) ->
    {ok, PrimaryProcID} = erlam_sched_sup:startup( SchedOpts ),
    erlam_sched:send(PrimaryProcID, {run,self(),Expression}),
    hang_for_result(). % Sleep until result message.

'spawn'( _Fun ) -> ok. %%XXX: DO THIS NOW!!!
    

%% @doc Validate the options and then start the scheduler on a particular 
%%   processor with a given implementation.
%% @end  
start_link( ProcID, SchedulerModule, Options ) -> 
    io:format("STARTING: ~p~n",[{ProcID,SchedulerModule,Options}]),
    case validate_options( Options ) of
        ok  ->
            Starter = self(), % Force sched to report back to us when started. 
            Args = [ Starter, ProcID, SchedulerModule, Options ],
            proc_lib:start_link( ?MODULE, init, Args );
        Err -> Err
    end.

%% @doc Stops a scheduler running on the particular Processor ID.
stop( ProcID ) ->
    pg:send( ?SCHEDULER_GROUP, {stop, ProcID} ).

%% @doc Stops all Erlam Schedulers.
stopall() ->
    pg:send( ?SCHEDULER_GROUP, stop ).

%% @doc Broadcast a message to all schedulers. Useful for synchronizing?
broadcast( Message ) ->
    pg:send( ?SCHEDULER_GROUP, {sched_msg, Message} ).

%% @doc Message a particular scheduler on a process ID. This could be useful 
%%   for passing a process between servers.
%% @end  
send( ProcID, Message ) ->
    pg:send( ?SCHEDULER_GROUP, {sched_msg, ProcID, Message}).

%% @doc Generates a default scheduler layout with one module per logical
%%   processing unit. This disregards exact topology and just returns
%%   the max list of modules. Note: schedulers can just call this function
%%   from their layout/2 function if they want to. 
%% @end
layout( Module, Topology, Options ) -> 
    Count = lists:foldl( fun(X,C)-> count_logical(X,0)+C end, 0, Topology ),
    ?DEBUG("IGNORING OPTIONS PASSED TO DEFAULT SCHEDULER: ~p~n",[Options]),
    lists:map(fun(N)-> {N,Module,option_update(N, Count, Options)} end, 
              lists:seq(0,Count-1)).
count_logical({logical,_},N)-> N+1;
count_logical({thread,L},N) -> recurse_logic(L,N);
count_logical({core,L},N)   -> recurse_logic(L,N);
count_logical({processor,L},N) -> recurse_logic(L,N).
recurse_logic(L,N) when is_list(L) -> 
    lists:foldl(fun(X,M)-> count_logical(X,M) end, N, L);
recurse_logic(X,N) -> count_logical(X,N).
option_update(N,Max,_Options)-> %TODO: Fix overriding options from user.
    [{primary,(N==0)},{processor,N,Max},{debug,true}].
    
%%% ==========================================================================
%%% Private API
%%% ==========================================================================

%% @hidden
%% @doc This function will not return until the local process gets a message.
%%   It's hoping that it's a result message from the primary scheduler.
%% @end
hang_for_result() ->
    receive 
        {result, Value} -> 
            Value;
        _Other -> 
            ?DEBUG("Got weird response to result manager: ~p~n", [_Other]),
            error
    end.

%% @private
%% @doc Initialize the user-space scheduler server by binding it to the Erlang
%%   VM scheduler and telling it not to keep track of stats about it.
%% @end
init( Starter, ProcID, Module, Options ) ->
    process_flag( scheduler, ProcID ), % Bind to Processor ID.
    process_flag( sensitive, true   ), % Remove all RT Stats gathering.
    process_flag( priority,  high   ), % Push the priority of the scheduler up.
    join_pg( ?SCHEDULER_GROUP ),       % Join a scheduler pool for msg passing.
    State = initial_state( ProcID, Module, Options ),
    server_entry( Starter, State ).    % Initialize the user-space scheduler.


%% @private
%% @doc Get the Scheduler options for the supervisor so that it can clean the
%%   input for you.
%% @end
sched_opts() -> ?SCHED_OPTS.  


%%% ==========================================================================
%%% Internal functionality
%%% ==========================================================================

-type internal_state() :: startup | running | stopping.

%% Internal state wrapper to keep scheduler behaviour working.
-record( is, {  
    state        :: internal_state(),  % The FSM state of the Sched Server.
    sched_proc   :: integer(),         % The current logical processor id.
    sched_module :: atom(),            % The scheduler behaviour implementation.
    sched_opts   :: scheduler_opts(),  % User-defined options for the scheduler.
    sched_state  :: scheduler_state(), % Scheduler state as returned from step.
    sched_status :: scheduler_status(),% The generalized scheduler status.
    status_hist  :: [ scheduler_status() ], % Historical data for visualization.
    mq           :: [ term() ]         % The current message queue.
} ).

%% @hidden
%% @doc Checks the user provided in options to verify they are useful. 
validate_options( Opts ) -> validate_options( Opts, ?SCHED_OPTS ).
validate_options( [H|T], Missing ) ->
    case valid_opt( H, Missing ) of
        {ok, NM} -> validate_options( T, NM );
        Err -> Err
    end;
validate_options( [], Missing ) ->
    case filter_optional( Missing ) of [] -> ok; [X|_] -> {missing, X} end.
valid_opt( X, Xs ) ->
    Valid = fun( P, S ) -> %% CHECKS IF optname AND size MATCH INCOMING OPTION 
        case is_tuple(X) of
            false -> (X =:= P andalso S == 0);
            true  -> (tuple_size(X) == S andalso element(1,X) == P)
        end
    end,
    Check = fun( {P,S,_} ) -> Valid(P,S); %% RUNS Valid FOR EACH OPTION
               ( {P,S}   ) -> Valid(P,S)
            end,
    case lists:partition( Check, Xs ) of
        {[],_} -> {badarg, X};
        {_,NXs} -> {ok, NXs}
    end.
filter_optional( L ) ->
    lists:filter(fun ({_,_,optional}) -> false;
                     (_) -> true
                 end, L ).

%% @hidden
%% @doc Return the default internal state.
initial_state( ProcID, Module, Options ) ->
    #is{ state = startup,
         sched_proc = ProcID,
         sched_module = Module,
         sched_opts = Options,
         mq = []
       }.

%% @hidden
%% @doc Starts off the server by calling 'init' on the behaviour then proceeding
%%   to the stepper loop. If there are any errors or warnings, those are taken
%%   care of by run_init/1.
%% @end  
server_entry( Starter, State ) ->
    case run_init( State ) of
        {ok, Updated} -> 
            % Report back to initalization code that it was a successful
            % startup of the scheduler system. We can now run the loop.
            proc_lib:init_ack( Starter, {ok, self()} ),
            hang_for_fin_init(),
            server_loop( Updated );
        {stop,Reason} -> 
            % Bad initialization, return error
            proc_lib:init_ack( Starter, {error, Reason}),
            server_stop( State )
    end.

%% @hidden
%% @doc Sleeps the scheduler transparently until all scheduler's have been
%%   initialized.
%% @end  
hang_for_fin_init() ->
    receive initack -> ok end.

%% @private
%% @doc Triggers the fin-init stage, which is done by erlam_sched_sup:startup/1.
init_ack() ->
    Members = pg:members( ?SCHEDULER_GROUP ),
    lists:map(fun (Pid) -> Pid!initack end, Members).

%% @hidden
%% @doc This is the stepper loop process for the internal scheduling behaviour.
%%   It handles calling the 'step' function and handling process updates.
%% @end  
server_loop( State ) ->
    Updated1 = check_messages( State ),
    Updated2 = run_step( Updated1 ),
    decide_future( Updated2 ).

%% @hidden
%% @doc Handles the internal scheduler cleanup.
server_stop( State ) ->
    run_cleanup( State ),
    exit(normal).
       
%% @hidden
%% @doc Using the public scheduler API calls, other schedulers can communicate
%%   with the local one. This will update the stateful message queue.
%% @end  
check_messages( #is{sched_opts=Opts} = State ) -> 
    N = get_message_buffer( Opts ),
    cm( State, N ). % Eat n messages at a time.
cm( State, 0 ) -> State;
cm( #is{sched_proc=ProcID, sched_opts=Opts} = State, N ) ->
    Wait = get_message_hang( Opts ),
    receive 
        {pg, _from, ?SCHEDULER_GROUP, {stop,ProcID}} ->  
            State#is{state=stopping};
        {pg, _from, ?SCHEDULER_GROUP, stop} -> 
            State#is{state=stopping};

        {pg, _from, ?SCHEDULER_GROUP, {sched_msg,ProcID,M}} -> 
            cm( add_mq( M, State ), N-1 );
        {pg, _from, ?SCHEDULER_GROUP, {sched_msg,M}} -> 
            cm( add_mq( M, State ), N-1 );

        _IGNORE_ -> cm( State, N-1 ) %TODO: Log?
    after Wait -> State end.

%% @hidden
%% @doc Checks the state determined by the stepper and message handlers, to 
%%   determine whether to stop the scheduler, or continue stepping.
%% @end
decide_future( #is{ state = stopping } = State ) -> server_stop( State );
decide_future( #is{ state = running } = State )  -> server_loop( State ).

%% @hidden
%% @doc Join a process group. Wraps creation as well (in case it doesn't exist).
join_pg( Group ) ->
    case pg:create( Group ) of
        ok -> pg:join( Group, self() );
        {error, already_created} -> pg:join( Group, self() );
        Err -> Err
    end.

%% ---------------------------
%% Behaviour callback wrappers

%% @hidden 
%% @doc Wraps the behaviour call to the implemented scheduler. Will handle
%%   contacting the runtime logger for posting warnings and errors.
%% @end  
run_init( State ) ->
    case catch 
        erlang:apply( State#is.sched_module, init, [State#is.sched_opts] )
    of
        {ok, S} -> {ok, State#is{ sched_state = S }};
        {warn, W, S} -> 
            erlam_rts:logger( warn, W ),
            {ok, State#is{sched_state = S, state = running}};
        {error, E} ->
            erlam_rts:logger( error, E ),
            {stop, E};
        {'EXIT',Reason} ->
            erlam_rts:logger( error, Reason ),
            {stop, Reason}
    end.

%% @hidden
%% @doc Wraps the behaviour call to the implemented scheduler.  
run_step( #is{ status_hist = Hist } = State ) ->
    case catch
        erlang:apply( State#is.sched_module, step, [State#is.state,
                                                    State#is.sched_state,
                                                    State#is.mq] )
    of 
        {ok, Status, Updated} ->  
            State#is{sched_status=Status, 
                     sched_state=Updated,
                     status_hist=[Status|Hist] %Note it's backwards
                    };
        {stop, Updated} ->
            State#is{state=stopping,
                     sched_state=Updated};
        {'EXIT', Reason} ->
            erlam_rts:logger(error, Reason),
            State#is{ state=stopping }
    end.

%% @hidden
%% @doc Wraps the behaviour call to the implemented scheduler. Will handle
%%   contacting the runtime logger for posting warnings and errors.
%% @end  
run_cleanup( State ) ->
    case catch 
        erlang:apply( State#is.sched_module, cleanup, [State#is.sched_state])
    of
        ok -> ok;
        {error, E} -> erlam_rts:logger( error, E );
        {warn, W}  -> erlam_rts:logger( warn, W  );
        {'EXIT',R} -> erlam_rts:logger( error, R )
    end.

%% ---------------------------
%% State modification

add_mq( M, #is{mq = MQ} = State ) -> State#is{mq= MQ++[M]}.
get_message_hang( Opts ) ->
    proplists:get_value( message_hang, Opts, ?DEFAULT_MESSAGE_HANG ).
get_message_buffer( Opts ) ->
    proplists:get_value( message_buffer, Opts, ?DEFAULT_MESSAGE_BUFFER ).

