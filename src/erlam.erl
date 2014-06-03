%% Compiler Driver and Interpreter Front-end
%%
%%  This module is called directly from our compiler scripts to initiate ErLam
%%  source compilation or an ErLam Shell. This module mainly focuses on option
%%  and argument parsing which will get correctly fed through to the other 
%%  modules.
%%
%% @author Alexander Dean
-module(erlam).
-export([main/1]).

-define(DEFAULT_OPTS, orddict:from_list([ 
                            {shell, false},    % Start up the ErLam Shell
%                            {to_forms, false}, % Generate Erlang AbsForms of els
%                            {to_erl, false},   % Generate Erlang Source of els
%                            {to_beam, false},  % Generate BEAM Module of els
                            {to_escript, true} % Generate Standalone Escript
                                        ])).

%% @doc The Main function called by the elsc script.
main([]) -> usage();
main(Args) ->
    {Files, Options} = check_options( Args, ?DEFAULT_OPTS ),
    case orddict:fetch( shell, Options ) of
        true -> shell( Options );
        false -> build( Files, Options )
    end.  

%% @hidden
%% @doc Actually performs the option parsing given the Arguments.
check_options(["--shell"|Rest], Opts) ->
    NOpts = orddict:store(shell, true, Opts),
    check_options( Rest, NOpts );
%check_options(["--to_forms"|Rest], Opts) ->
%    NOpts = orddict:store(to_forms, true, Opts),
%    check_options( Rest, NOpts );
%check_options(["--to_erl"|Rest], Opts) ->
%    NOpts = orddict:store(to_erl, true, Opts),
%    check_options( Rest, NOpts );
%check_options(["--to_beam"|Rest], Opts) ->
%    NOpts = orddict:store(to_beam, true, Opts),
%    check_options( Rest, NOpts );
check_options(["--no_escript"|Rest], Opts) -> %DEFAULT, so toggle off
    NOpts = orddict:store(to_escript, false, Opts),
    check_options( Rest, NOpts );

%%TODO: Add more ELIB references.
check_options(["--"|Files], Opts) -> {Files, Opts};
check_options( Files, Opts ) -> {Files, Opts}.

%% @private
%% @doc Enter the erlam shell.
shell( Cfgs ) -> 
    io:put_chars("Now entering ErLam Shell!\n" ++
                 "End all expressions with a ';'. Use ^C or 'q' to quit.\n"),
    erlam_interp:shell( Cfgs ).

%% @private
%% @doc Build ErLam code into a stand alone.
build( Files, Options ) -> 
    io:format("Compiling: ~s~n",[pbuild(Options)]),
    erlam_compile:files( Files, Options ).

%% @private
%% @doc Print help for a particular option, or the all of the man page.
usage() ->
    Usage = 
        "usage: elsc [options] file ...\n" ++
        "Options:\n" ++
        "--shell\t start the ErLam Interpreter\n" ++
%        "--to_forms\t generate intermediate Erlang Abstract Form\n" ++
%        "--to_erl\t generate the Erlang representation of ErLam\n"  ++
%        "--to_beam\t generate a BEAM Module to load by Erl VM\n"    ++
        "--no_escript\t toggle off the standalone EScript generation\n",
    io:put_chars(Usage).

%% @private
%% @doc Pretty print the options for file compilation.
pbuild( Opts ) ->
    Out = 
%        pm(to_forms,Opts)   ++"to_forms " ++
%        pm(to_erl,Opts)     ++"to_erl "   ++
%        pm(to_beam,Opts)    ++"to_beam "  ++
        pm(to_escript,Opts) ++"to_escript",
    lists:flatten(Out).

%% @hidden
%% @doc Pretty prints the options for intermediate forms.
pm(Name, Options) -> 
    case orddict:fetch(Name, Options) of
        true  -> "+"; false -> "-"
    end.

