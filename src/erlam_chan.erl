%%
%% ErLam Swap Channel
%%  This is the implementation of the Erlam Swap Channel process. It is created
%%  by the erlam_chan_serve and will automatically synchronize parallel
%%  accesses.
%%
-module(erlam_chan).
-behaviour(gen_server).
-include("debug.hrl").
-include("erlam_exp.hrl").
-include("erlam_chan.hrl").

%% API
-export([ start_link/1, swap/3, valid/1 ]).

%% gen_server callbacks
-export([ init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3 ]).

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

%% @doc Starts the server
-spec start_link( integer() ) -> {ok, pid()}.
start_link( ID ) -> 
    gen_server:start_link( ?MODULE, [ ID ], [] ). % Not registered.

%% @doc Given a channel, hang until another process swaps with you.
-spec swap( #chan{}, erlam_val(), reference() ) -> blocked | erlam_val().
swap( #chan{cpid=CPID}, Val, ProcessID ) ->
    gen_server:call(CPID, {swap, Val, ProcessID}, infinity).

%% @doc Checks to make sure it is a valid 
-spec valid( term() ) -> boolean().
valid( Chan ) when is_record( Chan, chan ) -> true;
valid( _ ) -> false.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Internal State
-record(state,{curid :: integer(), curval = {unblocked, dict:new()}}).

%% @private
%% @doc Initializes the server
init([ ID ]) -> 
    {ok, #state{curid=ID}}.

%% @private
%% @doc Handling synch call messages.
handle_call({swap, MyVal, Me}, _, #state{curval={V, D}, curid=ID}=State ) ->
%    ?DEBUG("SWAP(~p): ~p -> ~p [~p]~n",[ID,{Me,MyVal}, V, D]),
    case check_dict( Me, V, D ) of
        {swap, OtherVal, NewCur} ->
            {reply, OtherVal, State#state{curval=NewCur}};
        false ->
            %% Wasn't buffered, so perform swap. If swap is waiting then
            %% get value and save your's in the buffer addressed to Other.
            %% Otherwise, add self to channel and get back 'blocked'.
            (case V of
                unblocked ->
                    ?STATE_LOG_BLOCK(ID, Me),
                     NewCur ={ {Me, MyVal}, D },
                    {reply, blocked, State#state{curval=NewCur}};
                {Me, _} -> {reply, blocked, State};
                {Other, OtherVal} ->
                    ?STATE_LOG_UNBLOCK(ID, Me),
                    NewCur = { unblocked, dict:store( Other, MyVal, D ) },
                    {reply, OtherVal, State#state{curval=NewCur}}
            end)
    end;
handle_call(Request, _From, State) ->
    ?ERROR("Channel","Unknown Call: ~p",[Request]), 
    {reply, ok, State}.

%% @private
%% @doc Handling async cast messages
handle_cast(shutdown, State) -> {stop, normal, State};
handle_cast(Msg, State) -> 
    ?ERROR("Channel","Unknown Cast: ~p",[Msg]), {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(_Info, State) -> {noreply, State}.

%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
terminate(_Reason, _State) ->  ok.

%% @private
%% @doc  Convert process state when code is changed. NOT USED.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%% =========================================================================
%%% Private Functionality
%%% =========================================================================

%% @doc If the process shows up much later, the channel will have buffered the
%%   other process's swapped value for it. If it's not present, just continue,
%%   otherwise, removed yourself from buffer and return.
%$ @end
check_dict( Me, V, D ) ->
    case dict:find( Me, D ) of
        error -> false; % No waiting value for proc.
        {ok, Val} -> % Waiting value for proc.
            NewCur = {V, dict:erase(Me,D)},
            {swap, Val, NewCur}
    end.

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


