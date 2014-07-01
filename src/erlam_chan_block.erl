%%
%% ErLam Swap Channel - Block & Return
%%  This is an implementation of the Erlam Swap Channel process. It is created
%%  by the erlam_chan_serve and will automatically synchronize parallel
%%  accesses. This implementation returns and expects the scheduler to 
%%  continuously check whether the process has been unblocked.
%%
-module(erlam_chan_block).
-behaviour(gen_server).
-include("debug.hrl").
-include("process.hrl").

%% gen_server callbacks
-export([ init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3 ]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Internal State
-record(state,{curid :: integer(), curstate = unblocked, valdict=dict:new()}).

%% @doc Initializes the server
init([ ID ]) -> {ok, #state{curid=ID}}.

%% @doc Handling synch call messages.
handle_call({swap, MyExp, MyProc}, _, State ) ->
    case check_dict( MyProc, State ) of
        {ok, Reply, NewState} -> {reply, Reply, NewState};
        false -> perform_swap( MyProc, MyExp, State )
    end;
handle_call(Request, _From, State) ->
    ?ERROR("Channel","Unknown Call: ~p",[Request]), 
    {reply, ok, State}.

%% @doc Handling async cast messages
handle_cast(shutdown, State) -> {stop, normal, State};
handle_cast(Msg, State) -> 
    ?ERROR("Channel","Unknown Cast: ~p",[Msg]), {noreply, State}.


%%% gen_server default callbacks, we do not use the following %%%
handle_info({scheduler,ID,Reply}, State) ->
    process_flag( scheduler, ID ),
    Reply!{ok, self()},
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) ->  ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% =========================================================================
%%% Private Functionality
%%% =========================================================================

%% @doc If the process shows up much later, the channel will have buffered the
%%   other process's swapped value for it. If it's not present, just continue,
%%   otherwise, removed yourself from buffer and return.
%$ @end
check_dict( #process{proc_id=Me}=P, #state{valdict=D}=S ) ->
    case dict:find( Me, D ) of
        error     -> % No waiting value for proc.
            false;
        {ok, Val} -> % Waiting value for proc.
            Reply = {unblocked, [ P#process{chan_val=Val} ] },
            NewState = S#state{valdict=dict:erase(Me,D)},
            {ok, Reply, NewState}
    end.
     
%% @doc Swap wasn't buffered, so perform swap. If swap is waiting then get 
%%  value and save your's in the buffer addressed to Other. Otherwise, add 
%%  self to channel and get back 'blocked'.
%% @end
perform_swap( MyProc, MyExp, #state{curstate=unblocked}=State )->
    % Process is unblocked, so add self to queue and block the process.
    MyID = MyProc#process.proc_id,
    NewCurState = {MyID, MyExp},
    Reply = { blocked, [MyProc] },
    {reply, Reply, State#state{curstate=NewCurState}};
perform_swap( #process{proc_id=Me}=MyProc, _, #state{curstate={Me,_}}=State)->
    % Came back too early, no process performed swap with Me.
    % So I'm still blocked, and the state of the queue is still blocked.
    Reply = { blocked, [MyProc] },
    {reply, Reply, State};
perform_swap( MyProc, MyExp, #state{curstate={ID,Val}, valdict=D}=State ) ->
    % A different process is here, so I can take their value and update
    % the value dictionary for when they come back and check for a value.
    NewValDict  = dict:store( ID, MyExp, D ),
    NewCurState = unblocked,
    Reply = {unblocked, [ MyProc#process{chan_val=Val} ]},
    {reply, Reply, State#state{curstate=NewCurState,valdict=NewValDict}}.

