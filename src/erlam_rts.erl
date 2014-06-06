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
-export([safe_hang/1]).

% PRIVATE 
-export([step/3]).

-define(DEFAULT_SCHED, [{scheduler, erlam_sched_global},
                        {options, []}
                       ]).
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
    erlam_chan_serve:stop().

%% @doc Message scheduling system with new process, will return an ErLam 
%%   integer for success checking.
%% @end
-spec safe_spawn( fun() ) -> integer().
safe_spawn( Fun ) -> safe_spawn( Fun, [] ).

%% @doc Spawn a new process with a particular environment as it's closure,
-spec safe_spawn( fun(), [tuple()] ) -> integer().
safe_spawn( Fun, ENV ) ->
%    ?DEBUG("SPAWN: ~p, ~p~n",[Fun, ENV]), 
    case erlam_sched:spawn( Fun, ENV ) of
        ok         -> erlam_state:inc_processes(),   1;
        {error, E} -> ?DEBUG("Spawn Error: ~p",[E]), 0
    end.

%% @doc Safely step a process and keep the local environment up to date.
%%   Errors are propagated back up to the scheduler for handling.
-spec safe_step( erlam_process() ) -> {ok, erlam_process()} 
                                    | {yield, erlam_process()}
                                    | {stop, erlam_process()}
                                    | {hang, erlam_process(), non_neg_integer()}
                                    | {error, Reason :: any()}.
safe_step( #process{ exp=F, env=E, proc_id=ProcID} = P ) ->
%    ?DEBUG("STEPPING: ~p~n",[F]),
    case ?is_blocked(P) of
        true -> {ok, P};
        false -> (case step( ProcID, F, E ) of
                      {ok, Next, NE} -> {ok, P#process{exp=Next,env=NE}};
                      {stop, Val} -> {stop, P#process{exp=Val}};
                      {hang, Val, -1} ->
                          {yield, P#process{exp=Val}};
                      {hang, Val, Sleep} -> 
                          Hang = {os:timestamp(), Sleep},
                          {hang, P#process{exp=Val,hang=Hang}, Sleep};
                      {hang, Val, NE, -1} ->
                          {yield, P#process{exp=Val,env=NE}};
                      {hang, Val, NE, Sleep} ->
                          Hang = {os:timestamp(), Sleep},
                          {hang, P#process{exp=Val,env=NE,hang=Hang}, Sleep};
                      {error, Reason} -> {error, Reason}
                  end)
    end.

%% @doc Put the process to sleep in the scheduler for at least X seconds.
-spec safe_hang( pos_integer() ) -> 1.
safe_hang( X ) ->
    Seconds = timer:seconds( X ),
    throw({sleep, Seconds}). %Handled upon return of step/2,

%%%===================================================================
%%% Private functionality
%%%===================================================================

%% @private
%% @doc Evaluate a single step of the erlam process. This is also utilized
%%   by the interpreter via a loop. This is really gross, so I only wanted to
%%   do it once.
%% @end
-spec step( reference(), erlam_exp(), [tuple()] ) ->
    {ok, erlam_exp(), [tuple()]} 
    | {halt, erlam_exp(), integer()} % Returns when the result of the step causes a sleep or block (hang/swap)
    | {stop, erlam_exp()} % Returns when the expression is a value, and can't be stepped further.
    | {error, any()}. % Returned only when there is a catasrophic error.
step( _, newchan, _ENV ) ->
    ChannelID = erlam_chan_serve:get_new_chan(),
    {stop, erlam_lang:new_chan( ChannelID )};
step( _, #erlam_erl{} = E, _ENV ) -> {stop, E};
step( _, #erlam_fun{} = F, _ENV ) -> {stop, F};
step( _, #erlam_chan{} = C, _ENV ) -> {stop, C};
step( _, N, _ENV ) when is_integer( N ) -> {stop, N};

step( _, #erlam_var{name=N}, ENV ) ->
    case lists:keyfind(N,1,ENV) of
        {N,V} -> {stop, V};
        false -> {error, badvar} 
    end;
step( ProcID, #erlam_if{exp=E1,texp=E2,fexp=E3} = IF, ENV ) ->
    case is_value( E1 ) of
        {true, 0} -> {ok, E3, ENV};
        {true, _} -> {ok, E2, ENV};
        false -> 
            (case interstep( ProcID, E1, ENV ) of
                {ok,NV,NENV}    -> {ok, IF#erlam_if{exp=NV}, NENV};
                {hang,NV,Sleep} -> {hang, IF#erlam_if{exp=NV}, Sleep};
                {hang,NV,NENV,Sleep} -> 
                     {hang, IF#erlam_if{exp=NV}, NENV, Sleep}
            end)
    end;
step( ProcID, #erlam_swap{chan=C,val=E}=Swap, ENV ) ->
    case is_value( C ) of
        {true, #erlam_chan{chan=Chan}} -> 
            (case is_value( E ) of
                 {true, _} -> 
                     (case erlam_chan:swap( Chan, E, ProcID ) of
                          blocked -> {hang, Swap, -1};
                          Val     -> {hang, Val, ENV, -1}
                      end);
                 false -> 
                     (case interstep( ProcID,  E, ENV ) of
                          {ok,NV,NENV}    -> {ok, Swap#erlam_swap{val=NV}, NENV};
                          {hang,NV,Sleep} -> {hang, Swap#erlam_swap{val=NV}, Sleep};
                          {hang,NV,NENV,Sleep} -> 
                                {hang, Swap#erlam_swap{val=NV}, NENV, Sleep}
                      end)
             end);
        {true, Unknown} -> 
            {error, {badchan, Unknown}};
        false ->
            (case interstep( ProcID, C, ENV ) of
                 {ok,NV,NENV}    -> {ok, Swap#erlam_swap{chan=NV}, NENV};
                 {hang,NV,Sleep} -> {hang, Swap#erlam_swap{chan=NV}, Sleep};
                 {hang,NV,NENV,Sleep} -> 
                     {hang, Swap#erlam_swap{chan=NV}, NENV, Sleep}
            end)
    end;
step( ProcID, #erlam_spawn{exp=E}=Spawn, ENV ) ->
    case is_value( E ) of
        {true, #erlam_fun{}} ->
            %% Valid Spawn of a function to another process. We keep to 
            %% semantics and will turn this expression into an application with
            %% nil. Which kickstarts the evaluation into the function.
            AppFun = #erlam_app{ exp1=E, exp2=0},
            {stop, erlam_rts:safe_spawn( AppFun, ENV )};
        {true, _} ->   % We fake that the error happened in the other thread,
            {stop, 0}; %  because spawn errors out if it's not a unit-function.
        false -> 
            (case interstep( ProcID, E, ENV ) of
                {ok,NV,NENV}    -> {ok, Spawn#erlam_spawn{exp=NV}, NENV};
                {hang,NV,Sleep} -> {hang, Spawn#erlam_spawn{exp=NV}, Sleep};
                {hang, NV, NENV, Sleep} ->
                              {hang, Spawn#erlam_spawn{exp=NV}, NENV, Sleep}
            end)
    end;
step( ProcID, #erlam_app{exp1=E1, exp2=E2}=App, ENV ) ->
    case is_value( E1 ) of
        {true, #erlam_fun{var=V,exp=E}} -> 
            (case is_value( E2 ) of
                 {true, _} ->
                     (case V of
                         nil_var -> {ok, E, ENV};
                         _ -> 
                            VarName = V#erlam_var.name,
                            {NewName, NewE} = check_and_clean(VarName, E, ENV),
                            {ok, NewE, [{NewName,E2}|ENV]}
                      end);
                 false -> 
                     (case interstep( ProcID, E2, ENV ) of
                         {ok, NV, NENV}    -> {ok, App#erlam_app{exp2=NV}, NENV};
                         {hang, NV, Sleep} -> {hang, App#erlam_app{exp2=NV}, Sleep};
                         {hang, NV, NENV, Sleep} ->
                              {hang, App#erlam_app{exp2=NV}, NENV, Sleep}
                     end)
             end);
        {true, #erlam_erl{arity=A,func=F}} ->
            (case is_value(E2) of
                 {true, _} ->
                     step_erl( A, F, E2, ENV ); 
                 false ->
                     (case interstep( ProcID, E2, ENV ) of
                         {ok, NV, NENV}    -> {ok, App#erlam_app{exp2=NV}, NENV};
                         {hang, NV, Sleep} -> {hang, App#erlam_app{exp2=NV}, Sleep};
                         {hang, NV, NENV, Sleep} ->
                              {hang, App#erlam_app{exp2=NV}, NENV, Sleep}

                     end)
             end);
        {true, _} -> {error, badapp};
        false -> 
            (case interstep( ProcID, E1, ENV ) of
                 {ok, NV, NENV}    -> {ok, App#erlam_app{exp1=NV}, NENV};
                 {hang, NV, Sleep} -> {hang, App#erlam_app{exp1=NV}, Sleep};
                 {hang, NV, NENV, Sleep} ->
                    {hang, App#erlam_app{exp1=NV}, NENV, Sleep}
            end)
    end.

%% @hidden
%% @doc Inter-step stepping. Assumes the next step is correct and possibly
%%   produces a value or new environment.
%% @end
interstep( ProcID, E, ENV ) ->
    case step( ProcID, E, ENV ) of
        {ok, NE}       -> {ok, NE, ENV};
        {ok, NE, NENV} -> {ok, NE, NENV};
        {stop, V}      -> {ok, V, ENV};
        {hang, V, Sleep} -> {hang,V,Sleep};
        {hang, V, NENV, Sleep} -> {hang, V, NENV, Sleep}
    end.

%% @hidden
%% @doc Assumes the Expression is a Value and will run the Erlang Function on
%%   it. If the Arity is non-1 then it will repackage it into an erlam_erl
%%   for reapplication to the next value.
%% @end
step_erl( A, F, E, _Env ) ->
    try 
        Res = F(E), % Run the Function on the Expression.
        case A of
            1 -> 
                (case erlam_lang:is_value( Res ) of
                     true -> {stop, Res} % Assert it is a valid Built-In. 
                 end);
            _ -> {stop, erlam_lang:new_erl( A-1, Res )}
        end
    catch 
        throw:{sleep,Time} -> % The function can safely hang by throwing
                              % A custom exception. We will return a special
                              % Event attached to the 'stop' request.   
            {hang, 1, Time};
        _:_ -> {stop, 0} 
    end.

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
parse_options( Opts ) -> parse_options( Opts, ?DEFAULT_RTS ).
parse_options( [], RTS ) -> RTS;
parse_options( ["-?"|_Rest], _ ) -> usage(), halt(0);
parse_options( ["-h"|_Rest], _ ) -> usage(), halt(0);
parse_options( ["-l"|_Rest], _ ) -> list_scheds(), halt(0); 
parse_options( ["-v"|Rest], {Sched, Opts} ) ->
    UpOpts = orddict:store(verbose, true, Opts),
    parse_options( Rest, {Sched, UpOpts}  );
parse_options( ["-o",Name|_Rest], _ ) -> list_sched_opts( Name );
parse_options( ["-s",Name|Rest], {Prev, Opts} ) ->
    {ok, NewSchedOpts} = verify_scheduler( Name, Prev ),
    parse_options( Rest,{NewSchedOpts, Opts} );
parse_options( [Unknown|_], _ ) ->
    io:format("Unknown runtime option: ~s~n",[Unknown]), 
    halt(1).

%% @hidden
%% @doc Print the usage information about the executable.
usage() ->
    EXEC = get_exec(),
    Usage = 
        "usage: "++EXEC++" [options] -- [scheduler_options]\n"++
        "Options:\n" ++
        "  -? | -h \t This help message.\n" ++
        "  -v\t\t Turn on verbose runtime message.\n" ++
        "  -l\t\t List all possible schedulers and a short description.\n" ++
        "  -s SCHED \t Select a scheduler to run the program with.\n"++
        "  -o SCHED \t Get runtime options for the particular scheduler.\n"++
        "\nScheduler Options:\n" ++
        "\tNote that scheduler options are utilized on a per scheduler basis,"++
        "\n\tto get a listing of them use the '-o' option above.\n",
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

%% @hidden
%% @doc Check if the module exists and is an erlam_scheduler implementer.
verify_scheduler( SchedName, Prev ) ->
    case erlam_schedulers:verify( SchedName ) of
        {ok, ModuleName} -> 
            {ok, lists:keystore(scheduler, 1, Prev, {scheduler,ModuleName})};
        {error,{missing,Fun,Arity}} ->
           io:format( "ERROR: Desired scheduler is missing callback: ~p/~p.~n",
                    [Fun, Arity]),
            halt(1);
        {error, _} ->
            io:format("ERROR: Scheduler '~s' does not exist on path.~n",
                      [SchedName]),
            halt(1)
    end.

%% @hidden
%% @doc Print out the list of native schedulers.
list_scheds() -> 
    Out = lists:foldl( fun({F,D},R)-> R ++ io_lib:format( "~p\t~s~n",[F,D]) end,
                       "Schedulers:\n", erlam_schedulers:list() ),
    io:put_chars( Out ).

%% @hidden
%% @doc Print out the list of scheduler options and then halt.
list_sched_opts( SchedName ) ->
    case erlam_schedulers:verify( SchedName ) of
        {ok, ModName} -> 
            (case erlam_schedulers:get_options( ModName ) of
                 false -> io:format("No Options Avaliable~n"),halt(0);
                 {ok,Opts} -> 
                     io:format("Scheduler Options:~n~p~n",[Opts]), halt(0)
             end);
        {error, _} -> 
            io:format("ERROR: Scheduler '~s' does not exist on path.~n",
                       [SchedName]),
             halt(1)
    end.

