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
                    NewCur ={ {Me, MyVal}, D },
                    {reply, blocked, State#state{curval=NewCur}};
                {Me, _} -> {reply, blocked, State};
                {Other, OtherVal} ->
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
%            ?DEBUG("SWAP SUCCESS~n"),
            NewCur = {V, dict:erase(Me,D)}, 
            {swap, Val, NewCur}
    end.
