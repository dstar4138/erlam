%% 
%% ErLam translations for IO
%%
%%  Converts the ErLam AST into either a Pretty-Print string or into raw
%%  string.
%%
-module(erlam_trans).

-include("debug.hrl").
-include("erlam_exp.hrl").
-include("erlam_chan.hrl").

-export([ast2pp/1, ast2src/1]).
%-export([ast2erlsrc/1, ast2forms/1]).

%% @doc Pretty print channels and functions by observing internals.
ast2pp( Exp ) ->
    Defs = [ { nil_var,     fun() -> "_" end },
             { newchan,     fun() -> "newchan" end},
             { integer,     fun integer_to_list/1 },
             { function,    fun(_) -> "..." end },
             { erlam_var,   fun atom_to_list/1 },
             { erlam_spawn, fun(E) -> ["^[",E,"]^"] end },
             { erlam_chan,  fun(C) -> ["[chan",C,"]"] end },
             { erlam_erl,   fun(A,_) -> ["[builtin/",A,"]"] end },
             { erlam_swap,  fun(C,V) -> ["(",C,")!(",V,")"] end }, 
             { erlam_fun,   fun(X,E) ->  ["(\\",X,"->",E,")"] end },
             { erlam_app,   fun(E1,E2) -> [ "(",E1," ",E2,")"] end }, 
             { erlam_if,    fun(E,T,F) -> ["case (",E,") of 0 => (",F,
                                                "); _ => (",T,") end"] end } ],
    rast( Exp, Defs ).


%% @doc Convert the AST to it's string form. This includes the Erlang source
%%   code in the #erlam_erl{} object.
%% @end  
ast2src( Exp ) ->
    Defs = [ { nil_var, fun() -> "nil_var" end },
             { newchan, fun() -> "newchan" end },
             { integer, fun integer_to_list/1 },
             { function, fun erl2src/1 },
             { erlam_var, fun(V) -> ["{erlam_var,'",atom_to_list(V),"'}"] end },
             { erlam_spawn, fun(E) -> ["{erlam_spawn,",E,"}"] end },
             { erlam_chan,  fun(C) -> ["[erlam_chan,",C,"}"] end },
             { erlam_erl,   fun(A,F) -> ["{erlam_erl,",A,",",F,"}"] end },
             { erlam_swap,  fun(C,V) -> ["{erlam_swap,",C,",",V,"}"] end }, 
             { erlam_fun,   fun(X,E) ->  ["{erlam_fun,",X,",",E,"}"] end },
             { erlam_app,   fun(E1,E2) -> ["{erlam_app,",E1,",",E2,"}"] end }, 
             { erlam_if,    fun(E,T,F) -> ["{erlam_if,",E,",",T,",",F,"}"] end }],
    rast( Exp, Defs ).

%%% ==========================================================================
%%% Recursive abstraction over ErLam AST
%%% ==========================================================================

%% @hidden
%% @doc Loops through the AST and performs type lookups for n
%%  but will otherwise recurse to implement the rest.
%% @end 
-spec rast( erlam_ast(), [{atom(), fun((...)->string())} ] ) -> string().
rast( nil_var, Funcs ) -> a( l( nil_var, Funcs ), [] );
rast( newchan, Funcs ) -> a( l( newchan, Funcs ), [] );
rast( I, Funcs ) when is_integer(I) -> a( l( integer, Funcs ), [I] );
rast( Fun, Funcs ) when is_function(Fun) -> a( l( function, Funcs ), [ Fun ] );
rast( #chan{id=ID}, Funcs ) -> a( l( integer, Funcs ), [ID]);
rast( #erlam_var{name=Var}, Funcs ) -> 
    a( l( erlam_var, Funcs ), [Var]);
rast( #erlam_app{exp1=E1,exp2=E2}, Funcs ) -> 
    a( l( erlam_app, Funcs ), [ rast(E1, Funcs), rast(E2, Funcs) ] );
rast( #erlam_if{exp=E, texp=T, fexp=F}, Funcs ) ->
    a( l( erlam_if, Funcs ),  [rast( E, Funcs ), rast(T, Funcs), rast(F, Funcs)] );
rast( #erlam_swap{chan=C, val=V}, Funcs ) ->
    a( l( erlam_swap, Funcs ), [ rast(C, Funcs), rast(V, Funcs) ] );
rast( #erlam_spawn{exp=E}, Funcs ) ->
    a( l( erlam_spawn, Funcs), [ rast(E, Funcs) ] );
rast( #erlam_fun{var=V,exp=E}, Funcs ) ->
    a( l( erlam_fun, Funcs ), [ rast(V, Funcs), rast(E, Funcs) ] );
rast( #erlam_chan{chan=C}, Funcs ) ->
    a( l( erlam_chan, Funcs ), [ rast(C, Funcs) ] );
rast( #erlam_erl{arity=I,func=F}, Funcs ) ->
    a( l( erlam_erl, Funcs ), [ rast(I,Funcs), rast(F,Funcs) ] ).

%% @hidden
%% @doc Shorthand for function application to a list of arguments.
a( F, A ) -> erlang:apply( F, A ).

%% @hidden
%% @doc Shorthand for proplist lookup and providing a default result.
l( T, L ) -> proplists:get_value( T, L ).


%% @hidden
%% @doc Convert erlang source code into it's raw string again.
erl2src( ErlCode ) ->
    {env, Env} = erlang:fun_info( ErlCode, env ),
    [AST|_] = lists:reverse( Env ),
    List = erl_syntax:form_list( AST ),
    Exp = erl_prettypr:format( List ),
    ["fun ",Exp," end"]. % Not sure why prettypr forgets surrounding fun/end?

