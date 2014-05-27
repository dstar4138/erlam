%%
%% ErLam Runtime System.
%%  This module is archived into the compiled script to be accessed alongside
%% the user's own code and the channel server. This sets everything up and
%% has the most likely chance of changing over time.
%%
%% @author Alexander Dean
-module(erlam_rts).
-include("debug.hrl").
-include("process.hrl").
-include("erlam_exp.hrl").

% PUBLIC
-export([setup/2,breakdown/0,safe_spawn/1,safe_spawn/2,safe_step/1]).

% PRIVATE 
-export([step/2]).

-define(DEFAULT_SCHED, []).
-define(DEFAULT_STATE,  orddict:from_list([
                            {verbose, false} % Print RTS Stats?
                                          ])).
-define(DEFAULT_RTS,{?DEFAULT_SCHED,?DEFAULT_STATE}).


%% @doc Start up the channel server and scheduling system.
-spec setup( [{atom(), any()}], term() ) -> term().
setup( RTSOptions, Expression ) ->
    {SchedOpts, StateOpts} = parse_options( RTSOptions ),
    %% Start the Channel server, will handle swapping and channel creation.
    erlam_chan_serve:start(),
    %% Start the Runtime Monitor, useful for global information gathering.
    erlam_state:start( StateOpts ),
    %% Intitialize the Schedulers and send the initial expression.
    erlam_sched:run( SchedOpts, Expression ).


%% @doc Shutdown channel server, and schedulers.
-spec breakdown() -> ok | {error, Reason :: any()}.
breakdown() ->
    %% Wait for the stopping of all global monitors
    erlam_state:stop(),
    %% Then kill the channels.
    erlam_chan:stop().


%% @doc Message scheduling system with new process, will return an ErLam 
%%   integer for success checking.
%% @end
-spec safe_spawn( fun() ) -> integer().
safe_spawn( Fun ) -> 
    safe_spawn( Fun, [] ).

-spec safe_spawn( fun(), [tuple()] ) -> integer().
safe_spawn( Fun, ENV ) ->
    ?DEBUG("SPAWN: ~p, ~p~n",[Fun, ENV]), 
    case erlam_sched:spawn( Fun, ENV ) of
        ok         -> erlam_state:inc_processes(),   1;
        {error, E} -> ?DEBUG("Spawn Error: ~p",[E]), 0
    end.

%% @doc Safely step a process and keep the local environment up to date.
%%   Errors are propagated back up to the scheduler for handling.
-spec safe_step( erlam_process() ) -> {ok, erlam_process()} 
                                    | {stop, erlam_process()}
                                    | {error, Reason :: any()}.
safe_step( #process{ exp=F, env=E} = P ) ->
    case ?is_blocked(P) of
        true -> {ok, P};
        false -> (case step( F, E ) of
                      {ok, Next} -> {ok, P#process{exp=Next}};
                      {ok, Next, NE} -> {ok, P#process{exp=Next,env=NE}};
                      {stop, Val} -> {stop, P#process{exp=Val}};
                      {error, Reason} -> {error, Reason}
                  end)
    end.


%%%===================================================================
%%% Private functionality
%%%===================================================================

%% @private
%% @doc Evaluate a single step of the erlam process. This is also utilized
%%   by the interpreter via a loop.
%% @end
step( newchan, _ENV ) ->
    Channel = #erlam_chan{ chan=erlam_chan_serve:get_new_chan() },
    {stop, Channel};
step( #erlam_erl{} = E, _ENV ) -> {stop, E};
step( #erlam_fun{} = F, _ENV ) -> {stop, F};
step( #erlam_chan{} = C, _ENV ) -> {stop, C};
step( N, _ENV ) when is_integer( N ) -> {stop, N};

step( #erlam_var{name=N}, ENV ) ->
    ?DEBUG("VAR:{~p,~p}~n",[N,ENV]),
    case lists:keyfind(N,1,ENV) of
        {N,V} -> {stop, V};
        false -> {error, badvar} 
    end;
step( #erlam_if{exp=E1,texp=E2,fexp=E3} = IF, ENV ) ->
    case is_value( E1 ) of
        {true, 1} -> {ok, E2};
        {true, _} -> {ok, E3};
        false -> 
            {NV, NENV} = interstep( E1, ENV ), 
            {ok, IF#erlam_if{exp=NV}, NENV}
    end;
step( #erlam_swap{chan=C,val=E}=Swap, ENV ) ->
    ?DEBUG("CHAN:{~p, ~p}~n",[C,E]),
    case is_value( C ) of
        {true, #erlam_chan{chan=Chan}} -> 
            (case is_value( E ) of
                 {true, _} -> {ok, erlam_chan:swap( Chan, E )};
                 false -> 
                     {NV, NENV} = interstep( E, ENV ),
                     {ok, Swap#erlam_swap{val=NV}, NENV}
             end);
        {true, Unknown} -> 
            {error, {badchan, Unknown}};
        false ->
            {NV, NENV} = interstep( C, ENV ),
            {ok, Swap#erlam_swap{chan=NV}, NENV}
    end;
step( #erlam_spawn{exp=E}=Spawn, ENV ) ->
    case is_value( E ) of
        {true, #erlam_fun{}} ->
            {stop, erlam_rts:safe_spawn( E, ENV )};
        {true, _} -> 
            {stop, 0}; % We fake that the error happened in the other thread.
        false -> 
            {NV, NENV} = interstep( E, ENV ),
            {ok, Spawn#erlam_spawn{exp=NV}, NENV}
    end;
step( #erlam_app{exp1=E1, exp2=E2}=App, ENV ) ->
    case is_value( E1 ) of
        {true, #erlam_fun{var=V,exp=E}} -> 
            (case is_value( E2 ) of
                 {true, _} ->
                     (case V of
                         nil_var -> {ok, E, ENV};
                         _ -> 
                            VarName = V#erlam_var.name,
                            {ok, E, [{VarName,E2}|ENV]}
                      end);
                 false -> 
                     {NV, NENV} = interstep( E2, ENV ),
                     {ok, App#erlam_app{exp2=NV}, NENV}
             end);
        {true, #erlam_erl{arity=A,func=F}} ->
            (case is_value(E2) of
                 {true, _} -> 
                     IsRes = (case A of 1 -> stop; _ -> ok end),
                     Res = (try F(E2) catch _:_ -> 0 end),
                     {IsRes, Res};
                 false ->
                     {NV, NENV} = interstep( E2, ENV ),
                     {ok, App#erlam_app{exp2=NV}, NENV}
             end);
        {true, _} -> {error, badapp};
        false -> 
            {NV, NENV} = interstep( E1, ENV ),
            {ok, App#erlam_app{exp1=NV}, NENV}
    end.

%% @hidden
%% @doc Inter-step stepping. Assumes the next step is correct and possibly
%%   produces a value or new environment.
%% @end
interstep( E, ENV ) ->
    case step( E, ENV ) of
        {ok, NE} -> {NE, ENV};
        {ok, NE, NENV} -> {NE, NENV};
        {stop, V} -> {V, ENV}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Parse the command line options passed into the compiled program.
parse_options( [] ) -> ?DEFAULT_RTS;
parse_options( ["-?"|_Rest] ) -> usage(), halt(0);
parse_options( ["-h"|_Rest] ) -> usage(), halt(0);
parse_options( ["-v"|Rest] ) ->
    {Sched, Dict} = parse_options( Rest ),
    {Sched, orddict:store(verbose, true, Dict)};
parse_options( [Unknown|_] ) ->
    io:format("Unknown runtime option: ~s~n",[Unknown]), 
    halt(1).

%% @hidden
%% @doc Print the usage information about the executable.
usage() ->
    EXEC = get_exec(),
    Usage = 
        "usage: "++EXEC++" [options]\n"++
        "Options:\n" ++
        "-? | -h\t This help message.\n" ++
        "-v\t Turn on verbose runtime message.\n",
    io:put_chars( Usage ).

%% @hidden
%% @doc Get the script name for printout.
get_exec() ->
    try escript:script_name() 
    catch _:_ -> "els" 
    end.

%% @hidden
%% @doc Check that this AST representation cannot be stepped further.
is_value( Exp ) ->
    case Exp of
        #erlam_erl{} -> {true, Exp};
        #erlam_fun{} -> {true, Exp};
        #erlam_chan{} -> {true, Exp};
        N when is_integer(N) -> {true, N};
        _ -> false
    end.

