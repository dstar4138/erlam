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

%% @doc Put the process to sleep in the scheduler for at least X seconds.
safe_hang( X ) ->
    Seconds = timer:seconds( X ),
    timer:sleep( Seconds ), % TODO: This should modify the current RTS.
    1.% Returns a valid Expression.


%%%===================================================================
%%% Private functionality
%%%===================================================================

%% @private
%% @doc Evaluate a single step of the erlam process. This is also utilized
%%   by the interpreter via a loop.
%% @end
step( newchan, _ENV ) ->
    ChannelID = erlam_chan_serve:get_new_chan(),
    {stop, erlam_lang:new_chan( ChannelID )};
step( #erlam_erl{} = E, _ENV ) -> {stop, E};
step( #erlam_fun{} = F, _ENV ) -> {stop, F};
step( #erlam_chan{} = C, _ENV ) -> {stop, C};
step( N, _ENV ) when is_integer( N ) -> {stop, N};

step( #erlam_var{name=N}, ENV ) ->
    case lists:keyfind(N,1,ENV) of
        {N,V} -> {stop, V};
        false -> {error, badvar} 
    end;
step( #erlam_if{exp=E1,texp=E2,fexp=E3} = IF, ENV ) ->
    case is_value( E1 ) of
        {true, 0} -> {ok, E3};
        {true, _} -> {ok, E2};
        false -> 
            {NV, NENV} = interstep( E1, ENV ), 
            {ok, IF#erlam_if{exp=NV}, NENV}
    end;
step( #erlam_swap{chan=C,val=E}=Swap, ENV ) ->
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
                         nil_var -> {ok, E};%, ENV};
                         _ -> 
                            VarName = V#erlam_var.name,
                            {NewName, NewE} = check_and_clean(VarName, E, ENV),
                            {ok, NewE, [{NewName,E2}|ENV]}
                      end);
                 false -> 
                     {NV, NENV} = interstep( E2, ENV ),
                     {ok, App#erlam_app{exp2=NV}, NENV}
             end);
        {true, #erlam_erl{arity=A,func=F}} ->
            (case is_value(E2) of
                 {true, _} ->
                     step_erl( A, F, E2 ); 
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

%% @hidden
%% @doc Assumes the Expression is a Value and will run the Erlang Function on
%%   it. If the Arity is non-1 then it will repackage it into an erlam_erl
%%   for reapplication to the next value.
%% @end  
step_erl( A, F, E ) ->
    try 
        Res = F(E), % Run the Function on the Expression.
        case A of
            1 -> 
                (case erlam_lang:is_value( Res ) of
                     true -> {stop, Res} % Assert it is a valid Built-In. 
                 end);
            _ -> {stop, erlam_lang:new_erl( A-1, Res )}
        end
    catch _:_ -> {stop, 0} end.

%% @hidden
%% @doc Check for the variable name in the Environment. If it exists, then 
%%   we need to update all instances of the variable name in the Expression
%%   with a new variable name.
%% @end   
check_and_clean( VarName, Exp, Env ) ->
    case loopCheck( VarName, Env ) of
        VarName -> {VarName, Exp};
        Other -> {Other, loopClean(VarName, Other, Exp)}
    end.
loopCheck( V, L ) -> 
    case lists:keysearch( V, 1, L ) of
        {value,_} -> loopCheck( rand_atom( V ), L );
        false     -> V
    end.
loopClean( O, N, Exp ) -> % Recursively clean old_Var with new_Var in Exp.
    LC = fun(X) -> loopClean( O, N, X ) end,
    case Exp of
        #erlam_app{ exp1=E1, exp2=E2 } -> 
            #erlam_app{exp1=LC(E1), exp2=LC(E2)};
        #erlam_fun{ var=V, exp=E } ->
            (case V of 
                 #erlam_var{name=O} -> Exp; 
                  _ -> #erlam_fun{var=V, exp=LC(E)}
             end);
        #erlam_if{exp=E,texp=T,fexp=F} ->
            #erlam_if{exp=LC(E),texp=LC(T),fexp=LC(F)};
        #erlam_swap{chan=C,val=E} ->
            #erlam_swap{chan=LC(C),val=LC(E)};
        #erlam_spawn{exp=E} ->
            #erlam_spawn{exp=LC(E)};
        #erlam_var{name=O} ->
            #erlam_var{name=N};
        _ -> Exp
    end.
rand_atom( V ) -> list_to_atom(atom_to_list(V)++"@").


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
%% @doc Check that this AST representation cannot be stepped further. This
%%  wraps the erlam_lang call for case pattern matching ease-of-use.
%% @end 
is_value( Exp ) ->
    case erlam_lang:is_value( Exp ) of
        true  -> {true, Exp};
        false -> false
    end.

