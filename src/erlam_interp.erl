%%
%% ErLam Interpreter
%%  Provides access to the compiler toolchain on a per-string basis without
%%  worrying about files and the runtime.
%%
%% @author Alexander Dean
-module(erlam_interp).
-include("debug.hrl").
-include("erlam_exp.hrl").
-include("process.hrl").

%% PUBLIC
-export([shell/1]).

%% PRIVATE
-export([interpret/2]).

-define(FREE_BUFFER,"").

%%% ======================================================================
%%% Public Shell Operations
%%% ======================================================================

%% @doc The Shell entry point.
shell( _Opts ) ->
    Lib = start_minimum_rts(),
    cmdline( Lib ).


%%% ======================================================================
%%% Private Shell Operations
%%% ======================================================================

%% @hidden
%% @doc Opposite of start_minimum_rts/0. Will stop the shell's loop too.
stop_shell()->
    erlam_chan_serve:stop(),
    io:format("Good bye!~n").

%% @hidden
%% @doc Start the minimal runtime system and return the loaded libraries.
start_minimum_rts() ->
    erlam_chan_serve:start(), %% Start the channel server.
    erlam_lib:inter_update( [] ). %% Return the default libraries.

%% @hidden
%% @doc Run the shell loop by passing along the state and current buffer.
cmdline( State ) -> cmdline( State, ?FREE_BUFFER ).
cmdline( State, Buffer ) ->
    InputString = update_buffer( Buffer ),
    case InputString of
        "q" -> stop_shell();
        _   -> cmdline( State, do_interpret( State, InputString ) )
    end.

%% @hidden
%% @doc If the buffer has content, then it's a continuation of the previous
%%   command. This is similar to how the Python Interp Shell functions.
%% @end   
get_cmd( "" ) -> "erlam> ";
get_cmd( _ ) -> "... ".

%% @hidden 
%% @doc Will update the buffer by checking the next entered line. This will
%%   hang for user input.
%% @end  
update_buffer( Buffer ) ->
    CMD = get_cmd( Buffer ),
    case io:get_line( CMD ) of
        {error, _} -> Buffer;
        eof  -> Buffer;
        Data -> trim( lists:flatten([Buffer,Data]) )
    end.


%%% ======================================================================
%%% Interpreter functionality
%%% ======================================================================

%% @hidden
%% @doc Wraps the interpret call with buffer handling.
do_interpret( State, String ) ->
    case finished( String ) of
        {ok, Content} -> 
            interpret( State, Content ),
            ?FREE_BUFFER;
        false -> 
            String %return String as new buffer.
    end.

%% @private
%% @doc Preform the actual interpret and print the result. It will also 
%%   catch any issues from parsing, tokenizing, etc.
%% @end
interpret( State, StringContent ) ->
    try
        {ok, Tokens, _EndLine} = erlam_lexer:string( StringContent ),
        {ok, ExprTree} = erlam_parser:parse( Tokens ),
        AST = State( ExprTree ), 
        run_ast( AST )
    catch 
        error:{badmatch,_} -> % Typically caused by parser, assume syntax issue 
            io:format("ERROR: Syntax error, please balance all parens.~n",[]);
        E:V -> % Unknown error, Print debug information
            io:format("ERROR: Unknown error occured. Debug information follows.~n"),
            io:format("~p:~p~n~p~n",[E,V,erlang:get_stacktrace()])
    end.

%% @private
%% @doc Evaluates the Extended Abstract Syntax Tree. It uses the standard
%%   stepping process the RTS uses, and then pretty prints the resulting value.
%% @end
run_ast( AST ) ->
   Result = stepall( AST ), 
   PPAST = erlam_trans:ast2pp( Result ),
   io:format("~s~n",[PPAST]).

%% @hidden
%% @doc Checks if a string ends in a ';'.
finished( S ) -> finished(S,"").
finished( [], _ ) -> false;
finished( [$;|_], S) -> {ok, lists:reverse(S)}; % Cut off the rest.
finished( [X|R],S) -> finished(R,[X|S]).

%% @hidden
%% @doc Trims all whitespace from the front and back of the expression.
trim( A ) -> re:replace( A, "(^\\s+)|(\\s+$)", "", [global,{return,list}] ).

%% @hidden
%% @doc Steps through an AST until completion, defaulting with an empty 
%%   enviroment.
%% @end
stepall( AST ) -> 
    case stepall( make_ref(), AST, [] ) of
        {ok, Val} -> Val;
        {error,Reason} ->
            io:format("ERROR: ~p~n",[Reason]),
            0 % Default to 0 return value.
    end.
stepall( ProcID, AST, ENV ) ->
    check_msgs(),
    case erlam_rts:step( ProcID, AST, ENV ) of
        {ok, NextAST} -> stepall( ProcID, NextAST, ENV );
        {ok, NAST, NENV} -> stepall( ProcID, NAST, NENV );
        {stop, Val} -> {ok, Val};
        {stop, Val, Sleep} -> % Instead of stopping, just hang and continue.
            timer:sleep( Sleep ),
            stepall( ProcID, Val, ENV ); 
        {error, Reason} -> {error, Reason}
    end.

%% @hidden
%% @doc Due to not having a scheduler, all scheduling messages will be picked
%%   up by the local process. This will handle them in a default manner.
%% @end
check_msgs() ->
    receive 
        {sched_spawn, Process} -> 
            basic_spawn( Process ),
            check_msgs()
        %TODO: Any other messages?
    after 0 -> ok end.

%% @hidden
%% @doc Rip the expression and environment out of the process and
%%   turn it into a basic Erlang process and let the Erlang scheduler take 
%%   care of it.
%% @end
basic_spawn( #process{ exp = E, env=Env, proc_id=ProcID } ) ->
   erlang:spawn( fun() -> stepall(ProcID, E, Env) end ).

