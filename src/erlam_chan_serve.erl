%%
%% ErLam Channel Server
%%  This is the server which keeps track of all open channels and will create
%%  new ones as needed by the runtime system. 
%%
-module(erlam_chan_serve).
-behaviour(gen_server).
-include("debug.hrl").
-include("erlam_chan.hrl").

%% API
-export([start/0, start/1, stop/0, get_new_chan/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Default Channel Module Implementation is blocking, but absorption can be
%%  toggled on using a runtime flag.
-define(DEFAULT_CHAN, erlam_chan_block).
-define(ABSORB_CHAN, erlam_chan_absorb).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server.
-spec start() -> ok.
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [?DEFAULT_CHAN], []), ok.

%% @doc Starts the server.
-spec start( orddict:orddict() ) -> ok.
start( Options ) -> 
    ChannelModule = get_channel_module( Options ),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ChannelModule], []), 
    ok.

%% @doc Stops the server and all swap channels safely.
-spec stop() -> ok.
stop() -> gen_server:cast(?MODULE, shutdown).

%% @doc Ask server for a channel to swap on. They are multi-use. 
-spec get_new_chan() -> #chan{}.
get_new_chan() -> gen_server:call(?MODULE, get_new_chan).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Internal State
-record(state,{curid :: integer(), open_chans :: dict(), mod :: atom()}).

%% @private
%% @doc Initializes the server
init([Mod]) -> 
    {ok, #state{curid=0, open_chans=dict:new(), mod=Mod}}.

%% @private
%% @doc Handling synch call messages
handle_call(get_new_chan, _From, S) ->
    {NewChan, NS} = build_handler( S ),
    {reply, NewChan, NS};
handle_call(Request, _From, State) ->
    ?ERROR("Channel","Unknown Call: ~p",[Request]), 
    {reply, ok, State}.

%% @private
%% @doc Handling async cast messages
handle_cast(shutdown, State) -> 
    kill_channels( State ),
    {stop, normal, State};
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


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Creates a new erlam_chan instance and links it with the current
%%   process. It then adds this instance to the dictionary of open 
%%   channels for later cleanup and possibly validity checking.
%% @end  
build_handler( #state{ curid=I, open_chans=M, mod=Mod } = S ) ->
    NewChan = #chan{ id=I, mod=Mod },
    case erlam_chan:start_link( NewChan ) of
        {ok, BuiltChan} ->
            CPID = BuiltChan#chan.cpid,
            NM = dict:store( I, CPID, M ),
            NewState = S#state{ curid=I+1, open_chans=NM },
            {BuiltChan, NewState};
        Error ->
            ?ERROR("Channel","Error building channel: ~p",[Error])
    end.


%% @hidden
%% @doc Loop over all open channels and send a shutdown command.
kill_channels( #state{ open_chans=Open } ) ->
    dict:map( fun(_ID, CPID) ->
                      gen_server:cast( CPID, shutdown )
              end, Open ).


%% @hidden
%% @doc Check to see if the absorption flag has been toggled
get_channel_module( OrdDict ) ->
    case orddict:find( absorption, OrdDict ) of
        {ok, true} -> ?ABSORB_CHAN;
         _ -> ?DEFAULT_CHAN
    end.

