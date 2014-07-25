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
    erlam_chan_serve:start( StateOpts ),
    %% Start the Runtime Monitor, useful for global information gathering.
    erlam_state:start( SchedOpts++StateOpts ),
    % Wrap Expression with passed in Argument Values
    AppliedExpression = apply_arguments( Expression, StateOpts ),
    %% Intitialize the Schedulers and send the initial expression.
    erlam_sched:run( SchedOpts, AppliedExpression ).

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
        ok         -> erlam_state:note( spawn ),     1;
        {error, E} -> ?DEBUG("Spawn Error: ~p",[E]), 0
    end.

%% @doc Safely step a process and log all reductions and yields.  
-spec safe_step( erlam_process() ) ->
      {ok, erlam_process()}          % On successful step
    | {stop, erlam_process()}        % When step returns value
    | {blocked, [erlam_process()]}   % When swap returns blocked
    | {unblocked, [erlam_process()]} % When swap returns successful
    | {hang, erlam_process()}        % When process asks for reschedule
    | {error, any()}.                % On unsuccessful step, error not handled
safe_step( P ) ->
%    ?DEBUG("STEPPING(~p): ~p~n",[P#process.proc_id,P#process.exp]),
    case ?is_hanging( P ) of
        true -> {hang, P};
        {false, #process{exp=F, env=E}=NP} -> % Removed hang from process. 
            case step( NP, F, E ) of
                {ok,_}=Ret -> erlam_state:note( reduction ), Ret;
                {blocked,_}=Ret -> erlam_state:note( yield ), Ret;
                {unblocked,_}=Ret -> erlam_state:note( yield ), Ret;
                Ret -> Ret
            end
    end.

%% @doc Put the process to sleep in the scheduler for at least X seconds.
-spec safe_hang( pos_integer() ) -> 1.
safe_hang( X ) ->
    Seconds = timer:seconds( X ),
    throw({sleep, Seconds}). %Handled upon return of step/2,

%%%===================================================================
%%% Private functionality
%%%===================================================================

apply_arguments( Expression, Opts ) ->
    Args = proplists:get_value(arguments,Opts,[]),
    Fun = fun(Var,Exp) -> erlam_lang:new_app(Exp, Var) end,
    lists:foldl( Fun, Expression, Args ).

%% @hidden
%% @doc Update a process with either a new expression or a both an expression
%%   and an environment.
%% @end  
push_proc( P, Exp ) -> P#process{exp=Exp}.
push_proc( P, Exp, Env ) -> P#process{exp=Exp,env=Env}.

%% @hidden
%% @doc For each continuation in the list, unwrap it by calling it with the
%%   given process (which will return an updated one). After we're finished
%%   set the process continuation set to empty.
%% @end
unwrap_cont( [], Proc ) -> Proc#process{cont=[]};
unwrap_cont( [C|R], Proc ) -> unwrap_cont( R, C(Proc) ).

%% @hidden
%% @doc Install a continuation into the given process.
install_cont( Cont, #process{cont=Cs}=P ) ->
    P#process{cont=[Cont|Cs]}.

%% @private
%% @doc Evaluate a single step of the erlam process. This is also utilized
%%   by the interpreter via a loop. This is really gross, so I only wanted to
%%   do it once.
%% @end
-spec step( erlam_process(), erlam_exp(), erlam_env() ) ->
      {ok, erlam_process()}          % On successful step
    | {stop, erlam_process()}        % When step returns value
    | {blocked, [erlam_process()]}   % When swap returns blocked
    | {unblocked, [erlam_process()]} % When swap returns successful
    | {hang, erlam_process()}        % When process asks for reschedule
    | {error, any()}.                % On unsuccessful step, error not handled
step( P, Exp, Env ) ->
    case interstep( P, Exp, Env ) of
        {ok,#process{cont=Cs}=NP} -> 
            {ok,unwrap_cont(Cs,NP)};
        {hang,#process{cont=Cs}=NP} -> 
            {hang,unwrap_cont(Cs,NP)};
        {blocked,ProcList} ->
            {blocked, ProcList};
        {unblocked,ProcList} ->
            NewProcList = lists:foldl( fun( #process{cont=Cs}=NP, A ) ->
                                              [unwrap_cont(Cs,NP)|A]
                                       end, [], ProcList ), 
            {unblocked, lists:reverse(NewProcList)};
        {stop, #process{cont=[]}=NP} -> {stop, NP};
        {stop, #process{cont=Cs}=NP} -> {ok, unwrap_cont(Cs, NP)};
        {error, Error} -> {error, Error}
    end.

%% @hidden
%% @doc Runs individual steps and unwraps the expression into continuations in
%%   the event a subexpression needs evaluation first. Continuations are saved
%%   in the process so that if they disappear (such as in a swap), they can be
%%   unwrapped at a later time.
%% @end
interstep( P, newchan, _ENV )       -> {stop, gen_channel(P)};
interstep( P, #erlam_erl{}, _ENV )  -> {stop, P};
interstep( P, #erlam_fun{}, _ENV )  -> {stop, P};
interstep( P, #erlam_chan{}, _ENV ) -> {stop, P};
interstep( P, N, _ENV ) when is_integer( N ) -> {stop, P};

interstep( P, #erlam_var{name=N}, ENV ) ->
    case lists:keyfind(N,1,ENV) of
        {N,V} -> {stop, push_proc(P,V)};
        false -> {error, {badvar,N}} 
    end;
interstep( P, #erlam_if{exp=E1,texp=E2,fexp=E3} = IF, ENV ) ->
    case is_value( E1 ) of
        {true, 0} -> {ok, push_proc(P,E3)};
        {true, _} -> {ok, push_proc(P,E2)};
        false ->
            Continuation = fun( NP ) ->
                    NV = NP#process.exp,
                    NP#process{exp=IF#erlam_if{exp=NV}}
            end,
            interstep( install_cont(Continuation, P), E1, ENV )
     end;
interstep( P, #erlam_swap{chan=C, val=E}=Swap, ENV ) ->
    case is_value( C ) of
        {true, #erlam_chan{chan=Chan}} -> 
            (case is_value( E ) of
                 {true, _} ->
                     Continuation=fun(NP)->
                            case NP#process.chan_val of
                                nil -> NP;
                                Val -> NP#process{exp=Val,chan_val=nil}
                            end
                     end,
                     % Only install continuation if process hasn't been blocked.
                     NP = (case ?is_blocked(P) of 
                               true -> P;
                               false-> install_cont(Continuation,P) 
                           end),
                     erlam_chan:swap( Chan, E, NP );
                 false -> 
                     Continuation = fun (NP) ->
                            NV = NP#process.exp,
                            NP#process{exp=Swap#erlam_swap{val=NV}}
                     end,
                     interstep( install_cont(Continuation, P), E, ENV )
             end);
        {true, Unknown} -> 
            {error, {badchan, Unknown}};
        false ->
            Continuation = fun(NP) ->
                NV = NP#process.exp,
                NP#process{exp=Swap#erlam_swap{chan=NV}}
            end,
            interstep( install_cont(Continuation, P), C, ENV )
    end;
interstep( P, #erlam_spawn{exp=E}=Spawn, ENV ) ->
    case is_value( E ) of
        {true, #erlam_fun{}} ->
            %% Valid Spawn of a function to another process. We keep to 
            %% semantics and will turn this expression into an application with
            %% nil. Which kickstarts the evaluation into the function.
            AppFun = #erlam_app{ exp1=E, exp2=0 },
            Val = erlam_rts:safe_spawn( AppFun, ENV ),
            {stop, push_proc(P, Val)};
        {true, _} ->   % We fake that the error happened in the other thread,
                       %  because spawn errors out if it's not a unit-function.
            {stop, push_proc(P, 0)}; 
        false -> 
            Continuation = fun( NP ) ->
                NV = NP#process.exp,
                NP#process{exp=Spawn#erlam_spawn{exp=NV}}
            end,
            interstep( install_cont(Continuation,P), E, ENV)
    end;
interstep( P, #erlam_app{exp1=E1, exp2=E2}=App, ENV ) ->
    case is_value( E1 ) of
        {true, #erlam_fun{var=V,exp=E}} -> 
            (case is_value( E2 ) of
                 {true, _} ->
                     (case V of
                         nil_var -> {ok, push_proc(P,E)};
                         _ -> 
                            VarName = V#erlam_var.name,
                            {NewName, NewE} = check_and_clean(VarName, E, ENV),
                            {ok, push_proc(P,NewE, [{NewName,E2}|ENV])}
                      end);
                 false ->
                    Continuation = fun( NP ) ->
                        NV = NP#process.exp,
                        NP#process{exp=App#erlam_app{exp2=NV}}
                    end,
                    interstep( install_cont(Continuation,P), E2, ENV )
             end);
        {true, #erlam_erl{arity=A,func=F}} ->
            (case is_value(E2) of
                 {true, _} ->
                     step_erl( P, A, F, E2, ENV ); 
                 false ->
                    Continuation = fun( NP ) ->
                        NV = NP#process.exp,
                        NP#process{exp=App#erlam_app{exp2=NV}}
                    end,
                    interstep( install_cont(Continuation,P), E2, ENV )
             end);
        {true, _} -> {error, badapp};
        false ->
            Continuation = fun( NP ) -> 
                 NV = NP#process.exp,
                 NP#process{exp=App#erlam_app{exp1=NV}}
            end,
            interstep( install_cont(Continuation,P), E1, ENV )
    end.

%% @hidden
%% @doc Assumes the Expression is a Value and will run the Erlang Function on
%%   it. If the Arity is non-1 then it will repackage it into an erlam_erl
%%   for reapplication to the next value.
%% @end
step_erl( P, A, F, E, _Env ) ->
    try 
        Res = F(E), % Run the Function on the Expression.
        case A of
            1 -> 
                (case erlam_lang:is_value( Res ) of
                     true -> {stop, push_proc(P, Res)};
                     false -> {ok, push_proc(P, Res)}
                 end);
            _ -> 
                Val = erlam_lang:new_erl( A-1, Res ),
                {stop, push_proc(P, Val)}
        end
    catch 
        throw:{sleep,Time} -> % The function can safely hang by throwing
                              % A custom exception. We will return a special
                              % Event attached to the 'stop' request.
            Hang = {os:timestamp(), Time},
            NP = push_proc(P, 1),
            {hang, NP#process{hang=Hang}};
        _:_ -> {stop, push_proc(P,0)} 
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
%% @doc Generates an Erlam_channel by calling the channel server and
%%   requesting a new channel be created. It then wraps it in a ErLam
%%   term for comparisons/equality checks.
%% @end
gen_channel(P) ->
    SelfID = erlam_sched:get_id(),
    ChannelID = erlam_chan_serve:get_new_chan( SelfID ),
    push_proc(P, erlam_lang:new_chan( ChannelID )).

%% @hidden
%% @doc Attempt a conversion from List (string) to integer. Halt if otherwise.
ltoi( L ) ->
    try list_to_integer( L )
    catch 
        _ -> io:format("ERROR: Can only pass in integers via args!"),
             halt(1)
    end.

%% @hidden
%% @doc Parse the command line options passed into the compiled program.
parse_options( Opts ) -> parse_options( Opts, ?DEFAULT_RTS ).
parse_options( [], RTS ) -> RTS;
parse_options( ["-?"|_Rest], _ ) -> usage(), halt(0);
parse_options( ["-h"|_Rest], _ ) -> usage(), halt(0);
parse_options( ["-l"|_Rest], _ ) -> list_scheds(), halt(0); 
parse_options( ["-r",ARGS|Rest], {Sched, Opts} ) -> 
    A = lists:map( fun ltoi/1, string:tokens( ARGS, "," ) ), 
    UpOpts = orddict:store(arguments,A,Opts),
    parse_options( Rest, {Sched, UpOpts} );
parse_options( ["-a"|Rest], {Sched, Opts} ) ->
    UpOpts = orddict:store(absorption,true,Opts),
    parse_options( Rest, {Sched, UpOpts} );
parse_options( ["-p",Name|Rest], {Sched, Opts} ) ->
    Value = verify_chanpin(Name),
    UpOpts = orddict:store(chanpin,Value, Opts),
    parse_options( Rest, {Sched, UpOpts} );
parse_options( ["-v"|Rest], {Sched, Opts} ) ->
    UpOpts = orddict:store(verbose, true, Opts),
    parse_options( Rest, {Sched, UpOpts}  );
parse_options( ["-o",Name|_Rest], _ ) -> list_sched_opts( Name );
parse_options( ["-s",Name|Rest], {Prev, Opts} ) ->
    {ok, NewSchedOpts} = verify_scheduler( Name, Prev ),
    parse_options( Rest,{NewSchedOpts, Opts} );
parse_options( ["--"|Rest], {Sched, Opts} ) ->
    NewSched = lists:keyreplace(options,1,Sched,{options,Rest}),
    {NewSched, Opts};
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
        "  -r ARGS\t\t Run Script applied with ARGS, separate w/ commas.\n"++
        "  -a\t\t Turn on channel absorption.\n" ++
        "  -p PTYPE \t Turn on channel pinning given a type of mechanic.\n"++
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
%% @doc Check to make sure the PTYPE entered is a channel pinning mechanic.
verify_chanpin( Value ) ->
    A = list_to_atom( Value ),
    case lists:member( A, erlam_chan:pinning_types()) of
        true -> A;
        false -> 
            io:format( erlam_chan:pinning_usage() ),
            halt(1)
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
                     io:format("Scheduler Options:~n~s~n",[Opts]), halt(0)
             end);
        {error, _} -> 
            io:format("ERROR: Scheduler '~s' does not exist on path.~n",
                       [SchedName]),
             halt(1)
    end.

