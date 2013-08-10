%%
%% After Parsing and Scanning, we have an Expression Tree, This tree needs a 
%% couple of updates that can be applied before continuing, The result of 
%% updating is ErLam's internal AST.
%% 
%% Modifications:
%%  - Changes some erlam_vars into erlam_erl (if they have been implemented
%%    in the library.
%%  - Expand some shorthand functions like `id`.
%%

-module(erlam_ast).
-export([update/1]).
-include("erlam_exp.hrl").

-define(FUN_VAR_NAME( Fun ), Fun#erlam_fun.var#erlam_var.name).

% Runs over the list of updates apply one after the next. This turns an ErLam
% expression into an internal ErLam AST with embedded Erlang code.
-spec update( erlam_exp() ) -> erlam_ast().
update( Expr ) ->
    lists:foldl(fun erlang:apply/2,
                Expr,
                [ % Note: Order matters (shorthand may expand with keywords).
                  fun expand_shorthand/1,
                  fun update_with_library/1 
                ]).

%% ===========================================================================
%% Update Functions!
%% ===========================================================================

% Using erlam_lib, update shorthand expressions such as `id`.
-spec expand_shorthand( erlam_ast() ) -> erlam_ast().
expand_shorthand( AST ) -> 
    F = fun( Var ) -> erlam_lib:shortvar(Var#erlam_var.name) end,
    astloop_repvar( AST, F, []).

% Using erlam_lib, update keyword's with their Erlang based code.
-spec update_with_library( erlam_ast() ) -> erlam_ast().
update_with_library( AST ) ->
    F = fun( Var ) -> erlam_lib:keyvar(Var#erlam_var.name) end,
    astloop_repvar( AST, F, [] ).



%% ===========================================================================
%% Utility Functions
%% ===========================================================================

% Loops through AST running F on all erlam_var objects to possibly replace
% them. It will ignore overrided vars if a function uses it as a name.
% See the above update functions for usage.
% @hidden
-spec astloop_repvar( erlam_ast(), fun(), [atom()] ) -> erlam_ast().
astloop_repvar( AST, _, _ ) when is_integer( AST ) -> AST;
astloop_repvar( AST, F, I ) when is_tuple( AST ) ->
    case element(1, AST) of
        erlam_var -> ignore_vars(AST, I, F);
        erlam_app -> AST#erlam_app{ exp1=astloop_repvar(AST#erlam_app.exp1,F,I),
                                    exp2=astloop_repvar(AST#erlam_app.exp2,F,I) };
        erlam_if  -> AST#erlam_if{ exp=astloop_repvar(AST#erlam_if.exp,F,I),
                                   texp=astloop_repvar(AST#erlam_if.texp,F,I),
                                   fexp=astloop_repvar(AST#erlam_if.fexp,F,I) };
        erlam_swap -> AST#erlam_swap{ val=astloop_repvar(AST#erlam_swap.val,F,I) };
        erlam_fun -> AST#erlam_fun{ exp=astloop_repvar( AST#erlam_fun.exp,
                                                       F,
                                                       [?FUN_VAR_NAME(AST)|I]
                                                     )}
    end.

% Performs the replacement if varname isn't member of the ignore list.
% @hidden 
-spec ignore_vars( erlam_var(), fun(), [atom()]) -> erlam_ast().
ignore_vars(AST, F, I) ->
    case lists:member(AST#erlam_var.name, I) of
        true -> AST;
        false -> F(AST)
    end.


