%%
%% Translating AST to Erlang source code.
%% 
-module(erlam_trans).
-export([to_erl/1]).
-include("debug.hrl").
-include("erlam_exp.hrl").

% Cleans up some things.
-define(build(S,P), io_lib:format(S,P)).

%% Translate an AST into Erlang Source code.
-spec to_erl( erlam_ast() ) -> string().
to_erl( AST ) ->
    % Thread Variable lookup through
    to_erl( AST, dict:new() ).

to_erl( N, _ ) when is_integer( N ) -> erlang:integer_to_list(N);
to_erl( newchan, _ ) -> "erlam_chan:get_new_chan()";
to_erl( nil_var, _ ) -> "_";
to_erl( #erlam_var{ name=X }, Vs ) -> 
    case dict:find( X, Vs ) of
        {ok, Val} -> Val;
        error -> ?ERROR("Translation","unknown var: ~p",[X])
    end;
to_erl( #erlam_app{ exp1=E1, exp2=E2 }, Vs ) -> 
    ?build("erlang:apply(~s,[~s])",[to_erl(E1,Vs), to_erl(E2,Vs)]);
to_erl( #erlam_if{ exp=Pred, texp=True, fexp=False }, Vs ) ->
    ?build("case (~s) of 0 -> (~s); _ -> (~s) end",[to_erl(Pred,Vs),
                                                    to_erl(True,Vs),
                                                    to_erl(False,Vs)]);
to_erl( #erlam_swap{ chan=C, val=V }, Vs ) ->
    ?build("erlam_chan:swap( ~s, ~s)",[to_erl(C,Vs), to_erl(V,Vs)]);
to_erl( #erlam_chan{ chan=N }, _ ) -> 
    ?build("{chan, ~s}", [erlang:integer_to_list(N)]);
to_erl( #erlam_spawn{ exp=E }, Vs ) ->
    ?build("erlam_rts:safe_spawn( ~s )", [to_erl(E,Vs)]);
to_erl( #erlam_fun{var=V, exp=E}, Vs ) ->
    {ok, X, NVars} = gen_new_var(V,Vs),
    ?build("fun( ~s ) -> (~s) end",[ X, to_erl( E, NVars ) ]);
to_erl( #erlam_erl{ func=F }, _ ) -> F.


%% ==========================================================================
%% Private Functions
%% ==========================================================================

% Generates a new variable for each one it comes across.
-spec gen_new_var( erlam_var(), dict() ) -> {ok, string(), dict()}.
gen_new_var( nil_var, Vars ) -> {ok, "_", Vars};
gen_new_var( #erlam_var{name=V}, Vars ) ->
    X = ?build("I_~s",[erlang:atom_to_list(V)]),
    {ok, X, dict:store(V,X,Vars)}.

