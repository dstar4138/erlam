%%
%% The ErLam Library. 
%%  Will read in all library files in ?LIB unless overridden amd will update
%%  an erlam progarm with the keywords and shorthands found.
%%
-module(erlam_lib).
-export([update/1,update/2,inter_update/1]).
-include("debug.hrl").
-include("erlam_exp.hrl").

-define(LIB, ["../lib/std.elib"]). %TODO: Move to a compiled header.

-type lib_cfg() 
             % Override where to parse lib files. 
            :: {libdir, string()} 
             % If user names var something thats in the libarary.
             | {error_on_override, boolean()}
             % If two defs from libs conflict (in different files).
             | {error_on_conflict, boolean()}.

%% Updates an Expression by replacing all keywords and shorthand with whats 
%% found in the libraries. By default it will only load std.elib from the lib
%% directory.
-spec update( erlam_exp() ) -> erlam_ast().
update( Exp ) -> update( Exp, [] ).

-spec update( erlam_exp(), [lib_cfg()] ) -> erlam_ast(). 
update( Exp, Cfg ) -> 
    replace_vars( Exp, get_libs(Cfg), [], Cfg ).

%% Interpreter Updater, pass it the configurations for the library and it will
%% return a function that will update an expression without reloading the 
%% libraries each time.
-spec inter_update( [lib_cfg()] ) -> fun().
inter_update( Cfg ) ->
    Lib = get_libs( Cfg ),
    fun (Exp) -> replace_vars( Exp, Lib, [], Cfg ) end.


%% ==========================================================================
%% Private Functions
%% ==========================================================================

%% Load and merge all of the libraries from config and returns a dictionary
%% of atom() to erlcode or erlam_exp.
%% @hidden
-spec get_libs( [lib_cfg()] ) -> dict().
get_libs( Cfgs ) ->
    Libs = search_default( libdir, Cfgs, ?LIB ),
    lists:foldl( load_merge(Cfgs), dict:new(), Libs ).

%% Returns a function depending on whether or not to error_on_conflicts. Merging
%% of libraries is first come first serve. Any duplicates are lost.
%% @hidden
-spec load_merge( [lib_cfg()] ) -> fun().
load_merge( Cfgs ) ->
    case search_default( error_on_confict, Cfgs, true ) of
        true -> fun error_merge/2;
        false -> fun noerror_merge/2
    end.
error_merge(Lib, CurDict) ->
    {ok, NewDict} = load_lib(Lib),
    dict:merge( fun(K,V1,_)->
                    ?ERROR("Update","Definition Conflict for ~p.",[K]),V1
                end,
        CurDict, NewDict).
noerror_merge(Lib, CurDict) ->
    {ok, NewDict} = load_lib(Lib),
    dict:merge( fun( _,V1,_)->V1 end,
        CurDict, NewDict).

%% Take a lib path and try to load it into a dictionary via the lexer and 
%% parser. (Lots of possible errors here.)
%% @hidden
-spec load_lib( string() ) -> {ok, dict()} | {error, Reason :: any()}.
load_lib( Lib ) -> %Eww...
    erlam_parser:parse(
        element(2, erlam_lexer:string( 
            erlang:binary_to_list( 
                    element(2, file:read_file(Lib)))))).


%% Recurse on the erlam_exp, and when you come upon vars, try to replace them.
%% In the event of a function, add the new-definition to the ignore list and
%% print an error message if its needed.
%% @hidden
-spec replace_vars( erlam_exp(), dict(), [atom()], [lib_cfg()]) -> erlam_ast().
replace_vars( E=#erlam_var{name=Var}, Lib, Ignore, _ ) ->
    case lists:member(Var, Ignore) of
        true -> E;
        false -> (case dict:find(Var, Lib) of error -> E; {ok, V} -> V end)
    end;
replace_vars( #erlam_fun{var=nil_var,exp=E}, L,I,C ) ->
    #erlam_fun{var=nil_var,exp=replace_vars(E,L,I,C)};
replace_vars( F=#erlam_fun{var=Var,exp=E}, L,I,C ) ->
    V = Var#erlam_var.name,
    case dict:is_key(V,L) of
        true -> (case search_default(error_on_override, C, false) of
                    true ->?ERROR("Update","User override of keyword: ~p",[V]);
                       _ -> ok
                 end);
        false-> ok
    end,
    F#erlam_fun{exp=replace_vars(E,L,[V|I],C)};
replace_vars( #erlam_app{exp1=E1,exp2=E2},L,I,C) ->
    #erlam_app{exp1=replace_vars(E1,L,I,C),
               exp2=replace_vars(E2,L,I,C)};
replace_vars( #erlam_if{exp=E,texp=T,fexp=F}, L,I,C)->
    #erlam_if{exp=replace_vars(E,L,I,C),
              texp=replace_vars(T,L,I,C),
              fexp=replace_vars(F,L,I,C)};
replace_vars( E=#erlam_swap{ val=V }, L,I,C ) ->
    E#erlam_swap{val=replace_vars(V,L,I,C )};
replace_vars( #erlam_spawn{ exp=E }, L,I,C) ->
    #erlam_spawn{ exp=replace_vars(E,L,I,C)};
replace_vars( E,_,_,_)-> E.


%% Wraps a config look up with a value check and will return default value
%% if it doesn't exist.
%% @hidden
-spec search_default( atom(), [lib_cfg()], any() ) -> any().
search_default( ErrName, Cfgs, Default) ->
    case lists:keyfind(ErrName, 1, Cfgs) of
        false -> Default;
        {ErrName, X} -> X
    end.
