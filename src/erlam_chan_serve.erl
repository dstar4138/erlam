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
-export([start/0, stop/0, get_new_chan/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server.
-spec start() -> ok.
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []), ok.

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
-record(state,{curid :: integer(), open_chans :: dict()}).

%% @private
%% @doc Initializes the server
init([]) -> {ok, #state{curid=0, open_chans=dict:new()}}.

%% @private
%% @doc Handling synch call messages
handle_call(get_new_chan, _From, S=#state{curid=I}) ->
    {CPID, NS} = build_handler( S ),
    {reply, #chan{id=I,cpid=CPID}, NS};
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
build_handler( #state{ curid=I, open_chans=M } = S ) ->
    case erlam_chan:start_link( I ) of
        {ok, CPID} ->
            NM = dict:store( I, CPID, M ),
            NewState = S#state{ curid=I+1, open_chans=NM },
            {CPID, NewState};
        Error ->
            ?ERROR("Channel","Error building channel: ~p",[Error])
    end.


%% @hidden
%% @doc Loop over all open channels and send a shutdown command.
kill_channels( #state{ open_chans=Open } ) ->
    dict:map( fun(_ID, CPID) ->
                      gen_server:cast( CPID, shutdown )
              end, Open ).

