%%
%% ErLam Interpreter
%%  Provides access to the compiler toolchain on a per-string basis without
%%  worrying about files and the runtime.
%%

-module(erlam_interp).
-export([shell/1]).

-export([interpret/2, run_erl/1, fun_wrap/1]).

shell( _Opts ) ->
    erlam_chan:start(),
    cmdline( erlam_lib:inter_update([]) ).
stop_shell()->
    erlam_chan:stop(),
    io:format("Good bye!~n").

cmdline(Lib) ->
    InputString = io:get_line("erlam> "),
    case InputString of
        ""  -> stop_shell();
        eof -> stop_shell();
        _ -> 
            interpret( InputString, Lib ),
            cmdline( Lib )
    end.

%TODO: merge this with compile_file (pretty much the same steps.
interpret( StringContents, Lib ) ->
    {ok, Tokens, _EndLine} = erlam_lexer:string( StringContents ),
    {ok, ExprTree} = erlam_parser:parse( Tokens ),
    AST = Lib( ExprTree ), 
    UserCode = erlam_trans:to_erl(AST),
    X = fun_wrap(UserCode),
%    io:format("~p~n",[X]),%Debug, but also interesting
    run_erl( X ).

run_erl( Code ) ->
    {ok, Ts, _} = erl_scan:string( Code ),
    {ok, Exps } = erl_parse:parse_exprs( Ts ),
    {value, Fun, _} = erl_eval:exprs( Exps, [] ),
    io:format("~p~n",[Fun()]).

fun_wrap( X ) -> lists:concat(["fun()-> ", X, " end."] ).
