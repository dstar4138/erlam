%%
%% ErLam Swap Channel - With Channel Absorption
%%  This is an implementation of the Erlam Swap Channel process. It is created
%%  by the erlam_chan_serve and will automatically synchronize parallel
%%  accesses. This implementation will hold onto the process which blocks the
%%  channel and will return it to the unblocking channel. 
%%
-module(erlam_chan_absorb).
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
%%    - CurVal is either unblocked, or {NewVal, BlockedProcess}.
-record(state,{curid :: integer(), curval = unblocked}).

%% @doc Initializes the server
init([ ID ]) -> {ok, #state{curid=ID}}.

%% @doc Handling synch call messages.
handle_call({swap, MyVal, Me}, _, #state{curval=Val}=State ) ->
    case Val of
        unblocked ->
            Reply = {blocked,[]},
            CurVal = {Me, MyVal},
            {reply, Reply, State#state{curval=CurVal}};

        {OtherProc, OtherVal} ->
            %% Generate our process list of unblocked processes and add
            %% the values they can replace with the swap expression.
            ProcList = [ Me#process{chan_val=OtherVal},
                         OtherProc#process{chan_val=MyVal} ],
            Reply = {unblocked, ProcList},
            CurVal = unblocked,
            {reply, Reply, State#state{curval=CurVal}}
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

