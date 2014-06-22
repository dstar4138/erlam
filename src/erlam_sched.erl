%% The ErLam Process Scheduler Server
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
-include("process.hrl").
-include("erlam_scheduler.hrl").

%PUBLIC
-export([run/2, spawn/2]).

%PUBLIC 
-export([return/1]).
-export([broadcast/1, send/2, check_mq/0, check_mq/1]).
-export([get_id/0,get_ids/0,is_primary/0]).

%PRIVATE
-export([start_link/4, stop/1, stopall/0]).
-export([init/5, init_ack/0]).

-define(DEFAULT_MESSAGE_HANG,    0). % Default is break immediately if nothing.
-define(DEFAULT_MESSAGE_BUFFER, -1). % Default is get everything

%% State logging happens after every tick and after startup. We assume that
%% the scheduler implementer kept to the standard status see:
%% include/erlam_scheduler.hrl for details. 
-define(LOG_STATE( Status ), erlam_state:log(get_id(), sched_state, Status)).

%%% ==========================================================================
%%% Public API
%%% ==========================================================================
%%%   These functions are utilized by the Runtime system (erlam_rts) to access
%%%   the loaded scheduling subsystem. The entry points are run/2 and spawn/2. 

%% @doc Send the initial expression to the primary processor and then waits
%%   for the result message. This function will "run" an expression using the
%%   scheduling options and nothing more.
%% @end  
run( SchedOpts, Expression ) ->
    {ok, PrimaryProcID, _Pid} = erlam_sched_sup:startup( SchedOpts ),
    spawn_to_primary(PrimaryProcID, Expression),
    init_ack(), %% Trigger the start of computation.
    hang_for_result(). %, % Sleep until result message.

%% @doc Spawn an Erlam function as a new process using the loaded scheduling
%%   behaviour. This implementation is kinda tricky: the process which calls
%%   this will be a scheduler (through safe_step/1 -> step/2 -> safe_spawn/1), 
%%   so we send an internal spawn message to self(), which it will read after
%%   the step has completed. This also has the effect of queuing the spawns.
%% @end
spawn( Fun, Env ) -> self() ! {sched_spawn, ?new_process( Fun, Env )}, ok.


%%% ==========================================================================
%%% Public Scheduler Server API
%%% ==========================================================================
%%%     These are ignored by most of the system besides the RTS and should not
%%%     be called directly. Instead these are utilized by the runtime system
%%%     calls to erlam_sched_sup:setup/2 via erlam_sched:run/2.

-define(SCHEDULER_GROUP, erlam_sched).

%% @private
%% @doc Validate the options and then start the scheduler on a particular 
%%   processor with a given implementation.
%% @end  
start_link( ProcID, PrimaryID, SchedulerModule, Options ) ->
        Starter = self(), % This behaviour works like most gen_*, and forces
                          % the implemented behavior to phone home after 
                          % init/1 completes with success/failure.
        Args = [ Starter, ProcID, PrimaryID, SchedulerModule, Options ],
        proc_lib:start_link( ?MODULE, init, Args ).

%% @private
%% @doc Triggers the fin-init stage, which is done by erlam_sched_sup:startup/1.
%%   Which in turn triggers the stepping of all schedulers in more or less
%%   lock-step.
%% @end
init_ack() ->
    Members = pg:members( ?SCHEDULER_GROUP ),
    lists:map( fun(Pid) -> Pid!initack end, Members ).

%% @private
%% @doc Stops a scheduler running on the particular Processor ID.
stop( ProcID ) ->
    pg:send( ?SCHEDULER_GROUP, {sched_internal, stop, ProcID} ).

%% @private
%% @doc Stops all Erlam Schedulers.
stopall() ->
    pg:send( ?SCHEDULER_GROUP, {sched_internal, stop} ).

    
%%% ==========================================================================
%%% Internal Scheduler API Functions
%%% ==========================================================================
%%%  These functions are provided for the scheduler behaviour to utilize for
%%%  either default implementations (e.g. layout/3) or to access inter-process
%%%  communication that the scheduler framework provides (e.g. send/2, 
%%%  check_mq/0).
%%%

%% Process Dictionary Values
%%   We save some values in the scheduler process dictionary for quick access
%%   and then abstract some callback functions for accessing and manipulating
%%   them. 
-define(STARTER, starter).
-define(PD_PRIMARYID, primaryid).
-define(PD_LPU_ID,lpuid).     %The logical processing units id this is bound to.
-define(PD_MSG_HANG,msghang). %How long until we resign hope on getting a msg.
-define(PD_MSG_MAX,msgmax).   %The max internals we may handle per step.
-define(PD_RAW_OPTS,rawopts). %The raw user options passed in, which fwd to init
-define(PD_STATUS,schedstate). %The internal state: startup/running/stopping
-define(PD_BEHAVE,schedbehaviour). %The scheduler behavior implementation module
-define(PD_SMQ,sched_msg_queue). %Buffered inter-sheduler communication.
-define(PD_TMQ,sched_internal_queue). %Buffered intra-thread communication.
-define(PD_STATUS_HIST, status_hist). %Historical data for visualization.

%% @doc Broadcast a message to all schedulers. Useful for synchronizing?
broadcast( Message ) ->
    pg:send( ?SCHEDULER_GROUP, {sched_msg, Message} ).

%% @doc Message a particular scheduler on a process ID. This could be useful 
%%   for passing a process between servers.
%% @end  
send( ProcID, Message ) ->
    pg:esend( ?SCHEDULER_GROUP, {sched_msg, ProcID, Message}).

%% @doc Check the local message queue (other side of send/2). Note that the
%%   only time this will valid is during a schedmodule:step/2 call.
%% @end 
check_mq() -> check_mq( get( ?PD_MSG_MAX ), false ).
check_mq( ForceHang ) -> check_mq( get(?PD_MSG_MAX), ForceHang ).
check_mq( N, ForceHang ) ->
    case get( ?PD_SMQ ) of
        [] -> check_mq_loop( N, ForceHang );
        [H|T] -> 
           put( ?PD_SMQ, T ), 
           {ok, H}
    end.
check_mq_loop( 0, false ) -> false;
check_mq_loop( N, ForceHang ) ->
    ProcID = get( ?PD_LPU_ID ), 
    Hang = get( ?PD_MSG_HANG ),
    receive 
        {pg_message, _from, ?SCHEDULER_GROUP, {sched_msg,ProcID,M}} -> 
            {ok, M};
        {pg_message, _from, ?SCHEDULER_GROUP, {sched_msg,_,_}} -> %Other LPU
             check_mq_loop( N-1, ForceHang );
        {pg_message, _from, ?SCHEDULER_GROUP, {sched_msg,M}} ->
            {ok, M};
        
        %%% INTERNAL MESSAGING, BUFFER/DISCARD AND CONTINUE %%%
        {crashed_member,?SCHEDULER_GROUP,_Pid} -> % We'll die too shortly.
            check_mq_loop( N-1, ForceHang );
        {new_member,?SCHEDULER_GROUP,_Pid} -> 
            check_mq_loop( N-1, ForceHang );
        {pg_message, _, _, {sched_internal, Message}} ->
            buffer(?PD_TMQ, Message),
            check_mq_loop( N-1, ForceHang );
        {pg_message, _, _, {sched_internal, Message, ProcID}} ->
            buffer(?PD_TMQ, Message),
            check_mq_loop( N-1, ForceHang );
        {pg_message, _, _, {sched_internal, _, _}} ->
            check_mq_loop( N-1, ForceHang );
        Unknown ->
            buffer(?PD_TMQ, Unknown),  % We would like to return timely, ignore 
            check_mq_loop( N-1, ForceHang ) % internals until after tick.
    after 
        Hang -> (case ForceHang of 
                     false -> false; 
                     true -> check_mq( N, true ) 
                 end)
    end.

%% @doc Get the calling scheduler's Logical Processor's ID or ProcID.
get_id() -> get( ?PD_LPU_ID ).

%% @doc Get a list of LPU IDs where scheduler's are bound to.
get_ids() ->
    Count = proplists:get_value( active,
                    supervisor:count_children( erlam_sched_sup )),
    lists:seq( 0, Count-1 ). %%TODO: Assuming layout is normalized

%% @doc Check if the current ID is equal to the primary id. This will tell us if
%%   the current LPU has been marked as primary by startup.
%% @end
is_primary() -> ( get(?PD_PRIMARYID) == get(?PD_LPU_ID) ).


%% @doc Return the value of the process as the result of the computation.
return( #process{ exp=Val, resrep=ResultAcceptor } ) 
    when is_pid( ResultAcceptor ) -> ResultAcceptor!{result,Val}.

%%% ==========================================================================
%%% Private Scheduler Process API
%%% ==========================================================================
%%% The erlam_sched module is the server which wraps around the particular 
%%% behaviour implementation. The following are the functions which implement
%%% the internal server behavior.
%%%

%% @private
%% @doc Initialize the user-space scheduler server by binding it to the Erlang
%%   VM scheduler and telling it not to keep track of stats about it, we can
%%   do that.
%% @end
init( Starter, ProcID, PrimaryID, Module, Options ) ->
    process_flag( scheduler, ProcID ), % Bind to Processor ID.
    process_flag( sensitive, true   ), % Remove all RT Stats gathering.
    process_flag( priority,  high   ), % Push the priority of the scheduler up.
    join_pg( ?SCHEDULER_GROUP ),       % Join a scheduler pool for msg passing.
    initial_state( ProcID, PrimaryID, Module, Options ),
    server_entry( Starter ).           % Initialize the user-space scheduler.

%% @hidden
%% @doc Load the internal process state.
initial_state( ProcID, PrimaryID, Module, Opts ) ->
    MsgHang = proplists:get_value( message_hang, Opts, ?DEFAULT_MESSAGE_HANG ),
    MsgMax = proplists:get_value( message_buffer, Opts, ?DEFAULT_MESSAGE_BUFFER ),
    put(?PD_PRIMARYID, PrimaryID),
    put(?PD_LPU_ID,ProcID),
    put(?PD_MSG_HANG, MsgHang),
    put(?PD_MSG_MAX, MsgMax),
    put(?PD_RAW_OPTS, Opts),
    put(?PD_STATUS, startup),
    put(?PD_BEHAVE, Module),
    put(?PD_SMQ, []),
    put(?PD_TMQ, []),
    put(?PD_STATUS_HIST, []).

%% @hidden
%% @doc Starts off the server by calling 'init' on the behaviour then proceeding
%%   to the stepper loop. If there are any errors or warnings, those are taken
%%   care of by run_init/1.
%% @end  
server_entry( Starter ) ->
    case run_init() of
        {ok, ImplState} -> 
            % Report back to initalization code that it was a successful
            % startup of the scheduler system. We can now run the loop.
            % However at this point, it will wait until the scheduler 
            % supervisor triggers the finish of init mode.
            proc_lib:init_ack( Starter, {ok, self()} ),
            hang_for_fin_init(),
            put(?PD_STATUS, running),
            ?LOG_STATE(startup),
            server_loop( startup, ImplState );
        {stop, Reason} -> 
            % Bad initialization, return error
            proc_lib:init_ack( Starter, {error, Reason}),
            exit({error, Reason})
    end.

%% @hidden
%% @doc Sleeps the scheduler transparently until all scheduler's have been
%%   initialized. See server_entry/1, init_ack/0 and erlam_sched_sup:startup/1.
%% @end  
hang_for_fin_init() ->
    receive
        initack -> ok;
        {new_member, _, _} -> % Ignore possible messages from pg when in setup
            hang_for_fin_init();
        {crashed_member,_,_} -> % Continue to crash
            io:put_chars("ERROR: Scheduler crashed.\n"),
            halt(1)
    end.

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

%% @hidden
%% @doc This is the stepper loop process for the internal scheduling behaviour.
%%   It handles calling the 'step' function and handling process updates.
%% @end  
server_loop( PrevStatus, ImplState ) ->
    case internal_handle_msgs( ImplState ) of
        {stop, Updated1} -> 
            ?LOG_STATE(stopped),
            server_stop( Updated1 );
        {ok, Updated1} ->
            {NextStatus, Updated2} = run_tick( PrevStatus, Updated1 ),
            ?LOG_STATE( NextStatus ),
            decide_future( NextStatus, Updated2 )
    end.

%% @hidden
%% @doc Handles the internal scheduler cleanup.
server_stop( ImplState ) ->
    run_cleanup( ImplState ),
    exit( shutdown ).
     
%% ---------------------------
%% Behaviour callback wrappers

%% @hidden 
%% @doc Wraps the behaviour call to the implemented scheduler. Will handle
%%   contacting the runtime logger for posting warnings and errors.
%% @end  
run_init( ) ->
    case catch 
        erlang:apply( get(?PD_BEHAVE), init, [get(?PD_RAW_OPTS)] )
    of
        {ok, ImplState} -> {ok, ImplState};
        {warn, W, ImplState} -> 
            erlam_rts:logger( warn, W ),
            {ok, ImplState};
        {error, E} ->
            erlam_rts:logger( error, E ),
            {stop, E};
        {'EXIT',Reason} ->
            erlam_rts:logger( error, Reason ),
            {stop, Reason}
    end.

%% @hidden
%% @doc Wraps the behaviour call to the implemented scheduler.  
run_tick( PrevStatus, ImplState ) ->
    erlam_state:note( tick ),
    case catch
        erlang:apply( get(?PD_BEHAVE), tick, [ PrevStatus, ImplState ] )
    of 
        {ok, Status, Updated} -> 
            put( ?PD_STATUS_HIST, [Status|get(?PD_STATUS_HIST)] ), %Note: backwards
            {Status, Updated};
        {stop, Updated} ->
            put( ?PD_STATUS, stopping ),
            {stop, Updated};
        {'EXIT', Reason} ->
            erlam_rts:logger(error, Reason),
            put( ?PD_STATUS, stopping ),
            {stop, ImplState}
    end.

%% @hidden
%% @doc Wraps the behaviour call to the implemented scheduler. Will handle
%%   contacting the runtime logger for posting warnings and errors.
%% @end  
run_cleanup( ImplState ) ->
    case catch 
        erlang:apply( get(?PD_BEHAVE), cleanup, [ ImplState ])
    of
        ok -> ok;
        {error, E} -> erlam_rts:logger( error, E );
        {warn, W}  -> erlam_rts:logger( warn, W  );
        {'EXIT',R} -> erlam_rts:logger( error, R )
    end.

%% @hidden
%% @doc Run the behavior's callback for spawning a process. Note that it
%%   calls it on the local running instance, which may effect how the 
%%   scheduler implemenation behaves.
%% @end
run_spawn_process( Process, ImplState ) ->
    case catch
        erlang:apply( get(?PD_BEHAVE), spawn_process, [ Process, ImplState ] )
    of
        {ok, NewState} -> {ok, NewState};
        {warn, W, NewState}  -> 
            erlam_rts:logger( warn, W  ),
            {ok, NewState};
        {error, E} -> erlam_rts:logger( error, E ), stop;
        {'EXIT',R} -> erlam_rts:logger( error, R ), stop
    end.
        
%% @hidden
%% @doc Check process mailbox for intra-thread communication. Buffer all 
%%  inter-scheduler communication for scheduler implementation.
%% @end 
internal_handle_msgs( ImplState ) ->
    Msgs = get(?PD_TMQ), put(?PD_TMQ,[]), % Get internal buffer, and override it
    ProcID = get(?PD_LPU_ID),
    Queue = flush_tmq( Msgs, ProcID ),
    handle_internal_queue( ImplState, Queue ).
flush_tmq( Queue, ProcID ) ->
    receive
        {pg_message, _, _, {sched_internal, Msg}} -> 
                        flush_tmq([Msg|Queue], ProcID);
        {pg_message, _, _, {sched_internal, Msg, ProcID}} -> 
                        flush_tmq([Msg|Queue], ProcID);
        {pg_message, _, _, {sched_internal, _, _}} -> % No match on ProcID 
                        flush_tmq( Queue, ProcID );
        {crashed_member,_Group,_Pid} -> % We'll die too shortly.
                        flush_tmq( Queue, ProcID );
        {new_member,_Group,_Pid} -> 
                        flush_tmq( Queue, ProcID );
        {sched_spawn, _}=Msg  -> 
                        flush_tmq([Msg|Queue], ProcID);

        %%% SCHEDULER MESSAGES, SO WE BUFFER IN MEMORY %%%
        {pg_message, _, _, {sched_msg, Msg}} -> % Scheduler message broadcast.
                        buffer(?PD_SMQ, Msg ),
                        flush_tmq( Queue, ProcID );
        {pg_message, _, _, {sched_msg, ProcID, Msg}} -> % Send msg to us
                        buffer( ?PD_SMQ, Msg ),
                        flush_tmq( Queue, ProcID );
        {pg_message, _, _, {sched_msg, _, _}} -> % Scheduler message not for us
                        flush_tmq( Queue, ProcID );
        _Other ->    
                        flush_tmq(Queue, ProcID)
    after 0 -> Queue end.
handle_internal_queue( ImplState, [] ) -> {ok, ImplState};
handle_internal_queue( ImplState, [H|R] ) ->
    case H of
        stop ->
           ?DEBUG("GOT STOP MESSAGE:~n"), 
            put(?PD_STATUS, stopping),
            {stop, ImplState};
        {sched_spawn, Process} ->
            (case run_spawn_process( Process, ImplState ) of
                 {ok, NewState} ->
                    handle_internal_queue( NewState, R );
                 stop -> 
                    put(?PD_STATUS, stopping),
                    {stop, ImplState}
            end);
        %TODO: Missing any internal messages?
        _ -> %Ignore
            ?DEBUG("Unknown internal mesage ~p~n",[H]),
            handle_internal_queue( ImplState, R )
    end. 


%% @hidden
%% @doc Checks the state determined by the stepper and message handlers, to 
%%   determine whether to stop the scheduler, or continue stepping.
%% @end
decide_future( stop, State ) -> server_stop( State );
decide_future( PrevStatus, State )  -> 
    %TODO: Log state before moving to next step.
    server_loop( PrevStatus, State ).

%% @hidden
%% @doc Join a process group. Wraps creation as well (in case it doesn't exist).
join_pg( Group ) ->
    case pg:create( Group ) of
        ok -> pg:join( Group, self() );
        {error, already_created} -> pg:join( Group, self() );
        Err -> Err
    end.


%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================
%%%     Functions utilized exclusively in this module.

%% @hidden
%% @doc Send the initial master function to the primary scheduler. 
spawn_to_primary( ProcessorID, Fun ) ->
    FakeEnv = [],
    Process = ?set_primary( ?new_process( Fun, FakeEnv ) ),
    Msg = {sched_internal, {sched_spawn, Process}, ProcessorID},
    pg:send( ?SCHEDULER_GROUP, Msg ).


%% @hidden
%% @doc Add a message to a particular message queue in the process dictionary.
buffer( MQ, Msg ) -> put( MQ, [ Msg | get(MQ) ] ).
