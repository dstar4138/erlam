%%
%% ErLam Channel Server
%%  This is the parallel channel handling server. 
%%
-module(erlam_chan).
-behaviour(gen_server).
-include("debug.hrl").
-include("erlam_exp.hrl").

%% API
-export([start/0, stop/0,get_new_chan/0,swap/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Private
-export([handler/2]).

-record(chan,{id :: integer()}).
-record(state,{curid :: integer(), open_chan :: dict()}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
-spec start() -> ok.
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []), ok.

%% @doc Stops the server
-spec stop() -> ok.
stop() -> gen_server:cast(?MODULE, shutdown).

%% @doc Ask server for a channel to swap on. They are multi-use. 
-spec get_new_chan() -> #chan{}.
get_new_chan() -> gen_server:call(?MODULE, get_new_chan).

%% @doc Given a channel, hang until another process swaps with you.
-spec swap( #chan{}, erlam_val() ) -> erlam_val().
swap( Chan, Val ) -> gen_server:call(?MODULE, {swap, Chan, Val}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([]) -> {ok, #state{curid=0, open_chan=dict:new()}}.

%% @private
%% @doc Handling synch call messages
handle_call(get_new_chan, _From, S=#state{curid=I}) ->
    {reply, #chan{id=I}, S#state{curid=I+1}};
handle_call({swap, #chan{id=ID}, Val}, From, S=#state{open_chan=Vals}) ->
    case dict:is_key(ID,Vals) of
        true -> 
            dict:fetch(ID,Vals)!{swap,From,Val}, 
            {noreply, S#state{open_chan=dict:erase(ID,Vals)}};
        false -> 
            NewHandler = spawn_handler(From, Val), 
            {noreply,S#state{open_chan=dict:store(ID,NewHandler,Vals)}}
    end;
handle_call(Request, _From, State) ->
    ?ERROR("Channel","Unknown Call: ~p",[Request]), {reply, ok, State}.

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
spawn_handler( FromPid, FirstVal ) ->
    spawn( erlam_chan, handler, [FromPid, FirstVal]).
handler( From, Val ) ->
    receive
        {swap,Other,OVal} ->
            gen_server:reply(Other,Val), 
            gen_server:reply(From,OVal);
        Other -> 
            ?ERROR("Channel","Handler received unexpected message: ~p",[Other]),
            handler( From, Val )
    end.
        
