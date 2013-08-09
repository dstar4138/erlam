-module(lexer_eunit_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-define(SCAN(S), erlam_lexer:string(S)).
-define(TOKEN(T,C), {ok, [{T, 1, C}], 1}).
-define(LEX_ERR(E), {error, E}).
-define(LEX_EOF(E), {eof, E}).

scan_integer_test_() ->
    [
        ?_assert(?SCAN("0") =:= ?TOKEN(integer,0)),
        ?_assert(?SCAN("~12") =:= ?TOKEN(integer,-12)),
        ?_assert(?SCAN("345") =:= ?TOKEN(integer,345))
        %TODO: make sure it fails on "~"
    ].

scan_variable_test_() ->
    [
        ?_assert(?SCAN("x") =:= ?TOKEN(var,x))
        %TODO: More tests?
    ].

scan_expr_test_() ->
    [
        %TODO: More tests    
    ].
