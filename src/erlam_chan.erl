%%
%% ErLam Swap Channel
%%  This is a wrapper around the channel representation whether it be 
%%  absorption or block/return. Thus multiple more implementations can be
%%  provided without changing stepping.
%%
-module(erlam_chan).

-include("erlam_exp.hrl").
-include("erlam_chan.hrl").
-include("process.hrl").

%% API
-export([ start_link/3, swap/3, valid/1, stop/1 ]).
%% PRIVATE
-export([ pinning_types/0, pinning_usage/0 ]).

%% When logging the state of the channel, the LPU has no meaning so we 
%% use it for talking about the Channel ID instead. There are two states
%% that a channel logs which is `channel_blocked` or `channel_unblocked`.
%% As the value of the event we log the process identifier that caused it. 
-define(STATE_LOG_BLOCK(Channel, ProcessID),
            erlam_state:log(Channel, channel_blocked, ref2int(ProcessID))).
-define(STATE_LOG_UNBLOCK(Channel, ProcessID),
            erlam_state:log(Channel, channel_unblocked, ref2int(ProcessID))).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the channel server and will return it once started.
-spec start_link( channel(), atom(), tuple() ) -> {ok, channel()} 
                                                | {error, any()}.
start_link( #chan{ id=ID, mod=MODULE } = Chan, PinType, ProcIDs) ->
    case 
        gen_server:start_link( MODULE, [ ID ], [] )
    of
        {ok, Pid} -> 
            Pinning = process_channel_pinning( Pid, PinType, ProcIDs, Chan ),
            {ok, Chan#chan{cpid=Pid, pin=Pinning}};
        Error -> Error
    end.

%% @doc Given a channel, hang until the channel returns whether the process
%%   is blocked or not. We perform a state-log here so the abstraction does
%%   not need to worry about it.
%% @end
-spec swap( channel(), erlam_val(), erlam_process() ) -> 
        {blocked, erlam_process()} | {unblocked, [ erlam_process() ]}.
swap( #chan{id=ID, cpid=CPID}, Exp, #process{proc_id=PID} = Process ) ->
    case
        gen_server:call(CPID, {swap, Exp, Process}, infinity)
    of
        {blocked, Blocked} -> 
            ?STATE_LOG_BLOCK(ID, PID),
            {blocked, set_state( blocked, Blocked )};
        {unblocked, Unblocked} -> 
            ?STATE_LOG_UNBLOCK(ID, PID),
            Marked = mark( ID, Unblocked ),
            {unblocked, set_state( running, Marked )}
    end.

%% @doc Checks to make sure it is a valid 
-spec valid( term() ) -> boolean().
valid( Chan ) when is_record( Chan, erlam_chan ) -> true;
valid( _ ) -> false.

%% @doc Shutdown the channel.
-spec stop( channel() ) -> ok.
stop(#chan{cpid=PID}) -> 
    gen_server:cast(PID, shutdown).


%%% =========================================================================
%%% Private API
%%% =========================================================================

%% @private
%% @doc Return a list of Channel pinning techniques for use in erlam_rts.
pinning_types() -> [none, same, even].%, freq].

%% @private
%% @doc Return a display message describing the provided pinning techniques.
pinning_usage() -> 
    "Channel Pinning Types (PTYPE):~n"++
    "none - (DEFAULT) Do not pin a channel to an LPU upon creation.~n"++
    "same - Pin the channel to the LPU which it was created on.~n"++
    "even - Pin the channel successively accross each running scheduler.~n"++
    "freq - Pin according to lpu usage by channel frequency (NOT IMPLEMENTED).~n".


%%% =========================================================================
%%% Private Functionality
%%% =========================================================================

%% @hidden
%% @doc Converts a Process ID (an erlang referencer) to an integer:
ref2int( Ref ) when is_reference( Ref ) ->
    erlang:list_to_integer(remove_non_ints( erlang:ref_to_list(Ref), [])).
remove_non_ints([],A)->lists:reverse(A);
remove_non_ints([$0|R],A)->remove_non_ints(R,[$0|A]);
remove_non_ints([$1|R],A)->remove_non_ints(R,[$1|A]);
remove_non_ints([$2|R],A)->remove_non_ints(R,[$2|A]);
remove_non_ints([$3|R],A)->remove_non_ints(R,[$3|A]);
remove_non_ints([$4|R],A)->remove_non_ints(R,[$4|A]);
remove_non_ints([$5|R],A)->remove_non_ints(R,[$5|A]);
remove_non_ints([$6|R],A)->remove_non_ints(R,[$6|A]);
remove_non_ints([$7|R],A)->remove_non_ints(R,[$7|A]);
remove_non_ints([$8|R],A)->remove_non_ints(R,[$8|A]);
remove_non_ints([$9|R],A)->remove_non_ints(R,[$9|A]);
remove_non_ints([_|R],A)->remove_non_ints(R,A).

%% @hidden
%% @doc Maintain order of processes, but update their blocked/running state.
set_state( _, [] ) -> [];
set_state( S, [H|T] ) -> [H#process{state=S}|set_state(S,T)].

%% @hidden
%% @doc Check whether we need to perform channel pinning by looking at the 
process_channel_pinning( ChannelPid, PinType, {ProcID, EvenID}, Chan ) ->
    case PinType of
        none -> % Alert scheduler even without LPU pinning
            erlam_sched:send( ProcID, {channel_pinning, Chan}),
            nil;
        same -> do_pin( ProcID, ChannelPid, Chan ), ProcID;
        even -> do_pin( EvenID, ChannelPid, Chan ), EvenID;
        freq -> 
            % TODO: Implement!
            do_pin( EvenID, ChannelPid, Chan ), EvenID
    end.
        
%% @hidden
%% @doc Pin the channel to the particular Erlam Scheduler. This is how we do 
%%   LPU pinning as all schedulers have been pinned separately to different 
%%   processors.
%% @end
do_pin( SchedulerID, ChannelPid, Channel ) -> 
    ChannelPid!{scheduler, SchedulerID, self()},
    erlam_sched:send( SchedulerID, {channel_pinning, Channel}),
    receive {ok, ChannelPid} -> ok end.

%% @hidden
%% @doc Quickly mark all returned processes with the name of the channel it
%%  just communicated with. Some schedulers may need this information for more
%%  detailed evaluation.
%% @end
mark( _, [] ) -> [];
mark( ID, [P|R] ) -> [ P#process{chan_id=ID} | mark( ID, R ) ].

