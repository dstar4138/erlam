%%
%% ErLam Interpreter
%%  Provides access to the compiler toolchain on a per-string basis without
%%  worrying about files and the runtime.
%%
%% @author Alexander Dean
-module(erlam_interp).
-export([shell/1]).

%% PRIVATE
-export([interpret/2, run_erl/1]).

-define(FREE_BUFFER,"").
-define(ANY_ERROR,_:_).

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
    erlam_chan:stop(),
    io:format("Good bye!~n").

%% @hidden
%% @doc Start the minimal runtime system and return the loaded libraries.
start_minimum_rts() ->
    erlam_chan:start(), %% Start the channel server.
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
            String
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
        UserCode = erlam_trans:to_erl(AST),
        run_erl( UserCode )
    catch ?ANY_ERROR ->
        io:format("ERROR: Syntax error, please balance all parens.~n",[])
    end.

%% @private
%% @doc Wraps the parsed code in a function call and converts it to Erlang.
%%   It then passes the Erlang expression to the underlying VM to process
%%   and print.
%% @end
run_erl( Code ) ->
    Wrapped = lists:concat(["fun()-> ", Code, " end."]),
    {ok, Ts, _} = erl_scan:string( Wrapped ),
    {ok, Exps } = erl_parse:parse_exprs( Ts ),
    {value, Fun, _} = erl_eval:exprs( Exps, [] ),
    io:format("~p~n",[Fun()]).

%% @hidden
%% @doc Checks if a string ends in a ';'.
finished( S ) -> finished(S,"").
finished( [], _ ) -> false;
finished( [$;|_], S) -> {ok, lists:reverse(S)}; % Cut off the rest.
finished( [X|R],S) -> finished(R,[X|S]).

%% @hidden
%% @doc Trims all whitespace from the front and back of the expression.
trim( A ) -> re:replace( A, "(^\\s+)|(\\s+$)", "", [global,{return,list}] ).

