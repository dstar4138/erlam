%  Scanner description file. Used by Erlang's Leex application to generate 
% erlam's lexical analyzer.
%
% @author Alexander Dean

Definitions.

% Reserved Keywords %
If = if
Swap = swap
Spawn = spawn
Fun = fun
Let = let
In  = in

% Reserved Symbols
OpenParen = \(
CloseParen = \)
Dot = \.
Comma = \,
Eq = \=

% User Data %
Integer = (~)?[0-9]+
Name = [A-Za-z\_\+\-\'][A-Za-z0-9\_]*

%% Ignore and trim all whitespace.
Wsp = \s*

Rules.

% Keywords/Symbols %
{If}    : mktoken( op_if,   TokenLine, TokenChars ).
{Swap}  : mktoken( op_swap, TokenLine, TokenChars ).
{Spawn} : mktoken( op_spawn,TokenLine, TokenChars ).
{Fun}   : mktoken( op_fun,  TokenLine, TokenChars ).
{Let}   : mktoken( op_let,  TokenLine, TokenChars ).
{In}    : mktoken( op_in,   TokenLine, TokenChars ).
{OpenParen}  : mktoken( op_open, TokenLine, TokenChars ).
{CloseParen} : mktoken( op_close, TokenLine, TokenChars ).
{Dot}   : mktoken(op_dot, TokenLine, TokenChars ).
{Comma} : mktoken(op_comma, TokenLine, TokenChars).
{Eq}    : mktoken(op_eq, TokenLine, TokenChars ).
{Wsp}   : skip_token.

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

mkname( Line, Chars ) -> {token, {var, Line, erlang:list_to_atom(Chars)}}.
