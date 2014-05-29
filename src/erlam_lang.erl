%% ErLam Language Abstraction
%%
%% All functions are aggressively inlined and use guards where possible. 
%%
-module(erlam_lang).
-compile([inline]).

%% General Debuggery
-include("debug.hrl").

%% Shared Internal Types
-include("erlam_chan.hrl").
-include("erlam_exp.hrl").

%% Public API
-export([new_var/1, new_fun/2, new_app/2, new_erl/2, new_chan/1]).
-export([is_expression/1, is_value/1]).

%%% ==========================================================================
%%% Standard Library Functionality
%%% ==========================================================================

%% @doc Make a new Erlam Variable given an Erlang Atom as a name.
new_var( V ) when is_atom(V) ->  #erlam_var{name = V}.


%% @doc Make a new Erlam Application given two Erlam Expressions. 
new_app( E1, E2 ) ->
    case is_expression( E1 ) andalso is_expression( E2 ) of
        true  -> #erlam_app{ exp1 = E1, exp2 = E2 };
        false -> error(badexp)
    end.

%% @doc Make a new Erlam Function given an Erlam Variable and Expression.
new_fun( V, E ) when is_record(V, erlam_var) ->
    case is_expression( E ) of
        true  -> #erlam_fun{ var = V, exp = E };
        false -> error(badexp)
    end.

%% @doc Make a new Erlam Built-In given an Integer representing the Arity and
%%   the pre-curried Erlang Function which implements it.
%% @end
new_erl( A, F ) when is_integer(A) andalso is_function(F) ->
    #erlam_erl{ arity = A, func = F }.

%% @doc Given the channel ID returned from erlam_chan_serve:get_new_chan/0, this
%%   creates the Erlam Channel object.
%% @end
new_chan( ID ) when is_record(ID, chan) ->
    #erlam_chan{ chan = ID }.

%% @doc Check if the term is an Erlam Expression.
is_expression( E )  when is_record(E, erlam_var)
                  orelse is_record(E, erlam_erl)
                  orelse is_record(E, erlam_fun)
                  orelse is_record(E, erlam_if)
                  orelse is_record(E, erlam_swap)
                  orelse is_record(E, erlam_chan)
                  orelse is_record(E, erlam_spawn)
                  orelse is_record(E, erlam_app) 
                  -> true;
is_expression( _ )-> false.

%% @doc Checks if the term is a valid value (i.e. cannot be stepped further).
is_value( E )  when is_record(E, erlam_erl)
             orelse is_record(E, erlam_fun)
             orelse is_record(E, erlam_chan)
             orelse is_integer(E)
              -> true;
is_value( _ ) -> false.


%%% ==========================================================================
%%%  Private Functionality
%%% ==========================================================================

%% @hidden
%% @doc Generate a random atom. Used for generating random variables.

