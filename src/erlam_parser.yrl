%   Parser description file. Used by Erlang's Yecc application to generate
% erlam's token parser.
%
% @author Alexander Dean

Nonterminals
    program
    exprs expr
    varlist
    vars
    if_expr swap_expr spawn_expr fun_expr let_expr int_expr var_expr.

Terminals
    op_if 
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
    var.

Rootsymbol program.
Endsymbol '$end'.

% A Program is currently just an Expression:
program -> exprs : flatten_apply( '$1' ).

%% Expression Application
exprs -> expr : ['$1'].
%exprs -> op_open exprs op_close : '$2'.
exprs -> expr exprs : ['$1'|'$2'].

%% Expression:
expr -> if_expr     : '$1'.
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
var_expr -> var : {erlam_var, value_of('$1'), 0}.

Erlang code.
%%
%% Private Help functions
%%

% Left-Asociative
flatten_apply( [E] ) -> E;
flatten_apply( [A,B|R] ) ->
    flatten_apply( [ {erlam_app, A, B} | R] ).
    
value_of( Token ) -> element(3, Token).
