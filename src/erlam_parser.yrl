%   Parser description file. Used by Erlang's Yecc application to generate
% erlam's token parser.
%
% @author Alexander Dean

Nonterminals
    file
    library
    program
    exprs expr
    varlist
    vars
    if_expr newchan_expr swap_expr spawn_expr fun_expr let_expr int_expr var_expr
    maps map mapexpr.

Terminals
    op_if
    op_newchan 
    op_swap
    op_spawn
    op_fun
    op_let
    op_in
    op_close
    op_open
    op_dot
    op_comma
    op_eq
    integer
    var
    nil_var
    op_openlib
    op_closelib
    op_erlcode
    op_erlblock
    op_obrack
    op_cbrack
    op_semi.

Rootsymbol file.
Endsymbol '$end'.

% A Parsed file is either a library or a program 
file -> library : '$1'.
file -> program : '$1'.

% A Library is a dictionary of expressions or erlcode 
library -> op_openlib maps : dict:from_list('$2').
maps -> map maps : ['$1'|'$2'].
maps -> op_closelib : [].

% Types of mapings are either erlcode or shorthand
map -> var_expr op_eq mapexpr : {name_of('$1'), '$3'}.
mapexpr -> expr op_semi : '$1'.
mapexpr -> op_erlcode op_obrack integer op_cbrack op_erlblock op_semi : 
    {erlam_erl, value_of('$3'), erlblock_eval( value_of( '$5' ) )}.


% A Program is currently just an Expression:
program -> exprs : flatten_apply( '$1' ).

%% Expression Application
exprs -> expr exprs : ['$1'|'$2'].
exprs -> expr : ['$1'].

%% Expression:
expr -> if_expr     : '$1'.
expr -> newchan_expr : '$1'.
expr -> swap_expr   : '$1'.
expr -> spawn_expr  : '$1'.
expr -> fun_expr    : '$1'.
expr -> let_expr    : '$1'.
expr -> int_expr    : '$1'.
expr -> var_expr    : '$1'.
expr -> op_open exprs op_close : flatten_apply('$2').

%% ========================================================================
%% NOTE: The following need to return records from include/erlam_exp.hrl
%% ========================================================================

% If:
if_expr -> op_if expr expr expr :
    {erlam_if, '$2', '$3', '$4'}.

% New Channel Call
newchan_expr -> op_newchan : newchan.

% Swap Channel:
swap_expr -> op_swap expr expr :
    {erlam_swap, '$2', '$3'}.

% Spawn Process:
spawn_expr -> op_spawn expr :
    {erlam_spawn, '$2'}.

% Function Definition :
fun_expr -> op_fun varlist op_dot expr :
    case length( '$2' ) of
        1 -> [V] = '$2', 
            {erlam_fun, V, '$4'};
        _ -> lists:foldr(fun (V,Acc) -> {erlam_fun, V, Acc} end,
                         '$4', 
                         '$2')
    end.
    
%   Variable List
varlist -> op_open vars op_close : '$2'.
varlist -> vars : '$1'.
vars -> var_expr op_comma vars : ['$1'|'$3'].
vars -> var_expr : ['$1'].

% Let: 
let_expr -> op_let var_expr op_eq expr op_in expr :
    {erlam_app, {erlam_fun, '$2', '$6'}, '$4'}. 


% Variable & Integer Expressions:
int_expr -> integer : value_of('$1').
var_expr -> var : {erlam_var, value_of('$1')}.
var_expr -> nil_var : nil_var.

Erlang code.
%%
%% Private Help functions
%%

% Left-Asociative
flatten_apply( [E] ) -> E;
flatten_apply( [A,B|R] ) ->
    flatten_apply( [ {erlam_app, A, B} | R] ).
    
name_of( Token ) -> element(2, Token).
value_of( Token ) -> element(3, Token).

% Parse builtin's by pass-through to Erlang Parser/Lexer
erlblock_eval( ErlBlock ) ->
    Wrapped = lists:concat([ErlBlock,"."]),
    {ok, Ts, _} = erl_scan:string( Wrapped ),
    {ok, Exps } = erl_parse:parse_exprs( Ts ),
    {value, Ex, _} = erl_eval:exprs( Exps, [] ),
    Ex.

