%%
%% Translating AST to Erlang source code.
%% 
-module(erlam_trans).
-export([to_erl/1, to_forms/2]).
-include("debug.hrl").
-include("erlam_exp.hrl").

% Cleans up some things.
-define(build(S), lists:concat(S)).
-define(list(Line,Item), {cons,Line,Item,{nil,Line}}).
-define(error(Line, ErrMsg), {call, Line, {remote,Line, {atom,Line,erlang},
                                                        {atom,Line,error},
                                                        [ErrMsg]}}).

%% @doc Translate an ErLam AST into Erlang Source code. This is used within the 
%%   ErLam Shell to quickly pass each expression through to the Erlang Shell.
%% @end  
-spec to_erl( erlam_ast() ) -> string().
to_erl( AST ) ->
    % Thread Variable Lookup-Dictionary through
    to_erl( AST, dict:new() ).

to_erl( N, _ ) when is_integer( N ) -> erlang:integer_to_list(N);
to_erl( newchan, _ ) -> "erlam_chan:get_new_chan()";
to_erl( nil_var, _ ) -> "_";
to_erl( #erlam_var{ name=X }, Vs ) -> 
    case dict:find( X, Vs ) of
        {ok, Val} -> Val;
        error -> ?ERROR("Translation","unknown var: ~p(~p)~n",[X,Vs])
    end;
to_erl( #erlam_app{ exp1=E1, exp2=E2 }, Vs ) -> 
    ?build(["erlang:apply(",to_erl(E1,Vs), ",[", to_erl(E2,Vs),"])"]);
to_erl( #erlam_if{ exp=Pred, texp=T, fexp=F }, Vs ) ->
    ?build(["case (",to_erl(Pred,Vs),") of 0 -> (",
                to_erl(F,Vs),"); _ -> (",to_erl(T,Vs),") end"]);
to_erl( #erlam_swap{ chan=C, val=V }, Vs ) ->
    ?build(["erlam_chan:swap(",to_erl(C,Vs),",", to_erl(V,Vs),")"]);
to_erl( #erlam_chan{ chan=N }, _ ) -> 
    ?build(["{chan, ", erlang:integer_to_list(N),"}"]);
to_erl( #erlam_spawn{ exp=E }, Vs ) ->
    ?build(["erlam_rts:safe_spawn(", to_erl(E,Vs), ")"]);
to_erl( #erlam_fun{var=V, exp=E}, Vs ) ->
    {ok, X, NVars} = gen_new_var(V,Vs),
    ?build(["fun(",X," ) -> (",to_erl(E,NVars),") end"]);
to_erl( #erlam_erl{ func=F }, _ ) -> F.


%% @doc Converts the Erlam AST into the internal Erlang Abstract Form. This 
%%   Can later be used to look closer at the CORE Erlang and to compile from.
%% @end  
-spec to_forms( erlam_ast(), integer() ) -> erl_parse:abstract_form().
to_forms( AST, Line ) -> 
    to_forms( AST, Line, dict:new() ).

to_forms( N, Line, _ ) when is_integer( N ) ->
    {integer, Line, N};
to_forms( newchan, Line, _ ) ->
    {call, Line, {remote, Line, {atom, Line, erlam_chan}, 
                                {atom, Line, gen_new_chan}, []}};
to_forms( nil_var, Line, _ ) -> 
    {var, Line, '_'};
to_forms( #erlam_var{ name=X}, Line, Vs ) ->
    case dict:find( X, Vs ) of
        {ok, Val} -> {var, Line, Val};
        error -> ?ERROR("Translation","unknown var: ~p(~p)~n",[X,Vs])
    end;
to_forms( #erlam_app{ exp1=E1, exp2=E2 }, Line, Vs ) ->
    {call, Line, {remote, Line, {atom, Line, erlang}, 
                                {atom, Line, apply}, 
                                [ to_forms( E1, Line, Vs),
                                  ?list(Line, to_forms( E2, Line, Vs))]}};
to_forms( #erlam_if{ exp=Pred, texp=T, fexp=F }, Line, Vs ) ->
    {'case', Line, to_forms(Pred, Line, Vs),
                   [{clause,Line,[{integer,Line,0}],[],[to_forms(F,Line,Vs)]},
                    {clause,Line,[{var,Line,'_'}],[],[to_forms(T,Line,Vs)]}]};
to_forms( #erlam_swap{ chan=C, val=V }, Line, Vs ) ->
    {call, Line, {remote, Line, {atom, Line, erlam_chan},
                                {atom, Line, swap},
                                [to_forms(C,Line,Vs), to_forms(V,Line,Vs)]}};
to_forms( #erlam_chan{ chan=N }, Line, _ ) -> 
    {tuple, Line, [chan, N]};
to_forms( #erlam_spawn{ exp=E }, Line, Vs ) ->
    SubForms = to_forms( E, Line, Vs ),
    {call, Line, {remote, Line, erlam_rts, safe_spawn, [ SubForms ]}};
to_forms( #erlam_fun{var=V, exp=E}, Line, Vs ) ->
    {ok, X, NVars} = gen_new_var(V,Vs),
    {'fun',Line, 
        {clauses, Line,[
            {clause, Line, [{var,Line,X}], [], [to_forms(E,Line,NVars)]}]}};
to_forms( #erlam_erl{ func=F }, Line, _ ) ->
    case erl_scan:string( F, Line ) of
        {ok, Tokens, _EndLoc} -> 
            (case erl_parse:parse_form( Tokens ) of
                 {ok, AbsForm}  -> AbsForm;
                 {error, Error} -> ?error(Line, Error)
             end);
        {error, Error} ->
            ?error(Line, Error)
    end.


%% ==========================================================================
%% Private Functions
%% ==========================================================================

% Generates a new variable for each one it comes across.
-spec gen_new_var( erlam_var(), dict() ) -> {ok, string(), dict()}.
gen_new_var( nil_var, Vars ) -> {ok, "_", Vars};
gen_new_var( #erlam_var{name=V}, Vars ) ->
    X = ?build(["I_",erlang:atom_to_list(V)]),
    {ok, X, dict:store(V,X,Vars)}.

