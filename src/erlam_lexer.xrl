%  Scanner description file. Used by Erlang's Leex application to generate 
% erlam's lexical analyzer.
%
% @author Alexander Dean

Definitions.

% Reserved Keywords %
If = if
NewChan = newchan
Swap = swap
Spawn = spawn
Fun = fun
Let = let
In  = in

% Library Keywords %
OpenLib = elib
CloseLib = bile
ErlCode = \_erl
ErlBlock = {[^}{]*}

% Reserved Symbols
OpenParen = \(
CloseParen = \)
Dot = \.
Comma = \,
Eq = \=
OpenBracket = \[
CloseBracket = \]
Semi = \;

% C Style Comments.
Comment = //[^\n\r]*

% User Data %
Integer = (~)?[0-9]+
Name = [A-Za-z\_\+\-\'][A-Za-z0-9\_]*

%% Ignore and trim all whitespace.
Wsp = (\s|\t|\n|\r)*

Rules.

% Keywords/Symbols %
{If}    : mktoken( op_if,   TokenLine, TokenChars ).
{NewChan} : mktoken( op_newchan, TokenLine, TokenChars ).
{Swap}  : mktoken( op_swap, TokenLine, TokenChars ).
{Spawn} : mktoken( op_spawn,TokenLine, TokenChars ).
{Fun}   : mktoken( op_fun,  TokenLine, TokenChars ).
{Let}   : mktoken( op_let,  TokenLine, TokenChars ).
{In}    : mktoken( op_in,   TokenLine, TokenChars ).
{OpenLib} : mktoken(op_openlib, TokenLine, TokenChars ).
{CloseLib} : mktoken(op_closelib, TokenLine, TokenChars ).
{ErlCode} : mktoken(op_erlcode, TokenLine, TokenChars ).
{ErlBlock} : mktoken(op_erlblock, TokenLine, prune( TokenChars, TokenLen )).
{OpenParen}  : mktoken( op_open, TokenLine, TokenChars ).
{CloseParen} : mktoken( op_close, TokenLine, TokenChars ).
{Dot}   : mktoken(op_dot, TokenLine, TokenChars ).
{Comma} : mktoken(op_comma, TokenLine, TokenChars).
{Eq}    : mktoken(op_eq, TokenLine, TokenChars ).
{OpenBracket} : mktoken( op_obrack, TokenLine, TokenChars ).
{CloseBracket} : mktoken( op_cbrack, TokenLine, TokenChars ).
{Semi} : mktoken( op_semi, TokenLine, TokenChars ).

{Wsp}   : skip_token.
{Comment} : skip_token.

% Data %
{Integer} : mkint( TokenLine, TokenChars ).
{Name} : mkname( TokenLine, TokenChars ).

Erlang code.

mktoken( Type, Line, Chars ) ->  {token, {Type, Line, Chars}}.

mkint( Line, Chars ) -> {token, {integer, Line, create_integer( Chars )}}.

create_integer( [126|R] ) -> % [126] == "~"
    - erlang:list_to_integer(R);
create_integer( X ) -> 
    erlang:list_to_integer(X).

mkname( Line, Chars ) ->
    case Chars of
        "_" -> {token, {nil_var, Line, "_"}};
        _ -> {token, {var, Line, erlang:list_to_atom(Chars)}}
    end.

prune( Chars, Len ) -> lists:sublist(Chars, 2, Len-2).
