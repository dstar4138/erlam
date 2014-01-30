%% 
%% ErLam Compiler Driver
%%  Drives the compilation process. Will provide hooks for dumping intermediate
%%  representations for review.
%%
-module(erlam_compile).

-include("gen_ebin.hrl").

-export([file/2, files/2]).

%% Compile Hooks
-export([to_forms/2, to_erl/2, to_beam/2, to_escript/2]).

%% @doc Compiles several files and displays the report according to the compiler
%%   options.
%% @end  
files( Files, Options ) ->
    Report = lists:foldl(fun(File,Report) -> [file(File,Options)|Report] end,
                         [], Files),
    display_report( Report, Options ).

%% @doc Compile a single file and returns a completion report.
file( Path, Options ) ->
    case read_file( Path ) of
        {ok, StringContents} -> 
            (case extract_code( StringContents ) of
                {ok, AST} -> 
                     NewOpts = orddict:store( path, Path, Options ),
                     push_to_files( AST, NewOpts );
                Err -> reportize("Error translating: "++Path, Err)
            end);
        Err -> reportize("Error parsing: "++Path, Err)
    end.

%%% ==========================================================================
%%% Compile Hook Functions
%%% ==========================================================================

%% @doc Takes an internal AST and converts it to Erlang Abstract Forms. If the
%%   option `path' is present, then it will save the forms to a new file along-
%%   side the file in `path'.
%% @end  
to_forms( AST, Options ) ->
    case erlam_trans:to_forms( AST ) of
        {ok, Forms} -> save_or_return( Forms, fun t2s/1, ".forms", Options );
        Err -> reportize( "Compiling Abstract Forms Failed", Err )
    end.

%% @doc Takes an internal AST and converts it to raw Erlang Source code. If the
%%   option `path' is present, then it will save the forms to a new file along-
%%   side the file in `path'.
%% @end  
to_erl( AST, Options ) ->
    Source = erlam_trans:to_erl( AST ),
    ID = fun(X)-> inject_module( wrap_for_source(X), Options ) end,
    save_or_return(Source, ID, ".erl", Options). 

%% @doc Takes an internal AST and converts it to an Erlang BEAM Module. If the
%%   option `path' is present, then it will save the forms to a new file along-
%%   side the file in `path'.
%% @end  
to_beam( AST, Options ) ->
    {ok, Forms} = to_forms( AST, [] ),
    ModForms = wrap_for_module(Forms, Options),
    io:format("~p~n",[ModForms]),
    ID = fun(X)-> X end,
    case compile:forms( ModForms, [binary, return] ) of
        {ok, _ModuleName, Binary, _Warn} -> 
            save_or_return( Binary, ID, ".beam", Options );
        {ok, _ModuleName, Binary} ->
            save_or_return( Binary, ID, ".beam", Options );
        Err -> reportize( "Compiling BEAM Failed", Err )
    end.

%% @doc Takes an internal AST and converts it to Erlang Binary EScript. If the
%%   option `path' is present, then it will save the forms to a new file along-
%%   side the file in `path'.
%% @end  
to_escript( AST, Options ) ->
    {ok, Erl} = to_erl(AST, []),
    Source = wrap_for_source( Erl ),
    Sections = [shebang, comment, {emu_args, 
                                    "+sbt s "++          % Max bind schedulers
                                    "+sbwt very_long "++ % Don't let them sleep
                                    "+scl false " ++     % Load Balancing off
                                    "-pa "++?EBIN_DIR},  % Link to see rts mod
                {source, list_to_binary(Source)}],
    case orddict:find( path, Options ) of
        error -> % Just Returning, So give back Contents.
            {ok, Source};
        {ok, Path} -> % Saving
            Save = determine_path( Path, ".ex" ),
            escript:create( Save, Sections ),
            "Successful save of: "++Save++"\n"
    end.


%%% ==========================================================================
%%% Internal Utility Functions
%%% ==========================================================================

%% @hidden
%% @doc Converts a term to String, see to_forms/2.
t2s( Term ) -> lists:flatten(io_lib:format("~p",[Term])).

%% @hidden
%% @doc Read the file contents into a string for tokenization.
read_file( Path ) ->
    case file:read_file( Path ) of
        {ok, Binary} -> {ok, erlang:binary_to_list(Binary)};
        {error, E} -> {error, E}
    end.

%% @hidden
%% @doc Convert the string contents into the Abstract Syntax Tree that we 
%%   use to store the ErLam code internally.
%% @end  
extract_code( StringContents ) ->
    try %TODO: Split up for better error messages.
        {ok, Tokens, _EndLine} = erlam_lexer:string( StringContents ),
        {ok, ExprTree} = erlam_parser:parse( Tokens ),
        AST = erlam_lib:update( ExprTree ),
        {ok, AST}
    catch _:Reason -> {error, Reason} end.

%% @hidden
%% @doc Converts the return values from each step into part of the progress
%%   report for displaying to the programmer.
%% @end  
reportize( Msg, {ok, _} ) -> io_lib:format("Success: ~s~n",[Msg]);
reportize( Msg, error ) -> io_lib:format("Error: ~s~n", [Msg]);
reportize( Msg, {error, E} ) -> io_lib:format("Error: ~s -> ~p~n",[Msg,E]);
reportize( Msg, {error, E, _} ) -> io_lib:format("Error: ~s -> ~p~n",[Msg,E]).

%% @hidden
%% @doc Push all lines of the report to the terminal.
display_report( [], _ ) -> ok;
display_report([Report|R], Opts) -> % TODO: Check verbosity options
    lists:map( fun (ok)->ok;
                   (L)->io:put_chars(L) 
               end,
               Report ),
    display_report(R,Opts).

%% @hidden
%% @doc Based on the options we can generate a BEAM, Erl Source, Erl Abstract
%%   Forms, and a standalone Escript.
%% @end  
push_to_files( AST, Options ) ->
  [ check_then_run( to_forms, Options, AST ),
    check_then_run( to_erl, Options, AST ),
    check_then_run( to_beam, Options, AST ),
    check_then_run( to_escript, Options, AST) ].

%% @hidden 
%% @doc Checks if a certain conversion function is needed and then applys it to 
%%   the AST.
%% @end  
check_then_run( Conv, Opts, AST ) ->
    case orddict:fetch( Conv, Opts ) of
        true -> erlang:apply( ?MODULE, Conv, [AST, Opts]);
        false -> ok
    end.

%% @hidden
%% @doc Saves the file if a path is set in the Options. It does so by first
%%   running the conversion function over the contents and then replacing the
%%   path's extention with the one given. If the `path' option is not given, 
%%   it just returns the Contents with no conversion.
%% @end  
save_or_return( Contents, Conv, Ext, Opts ) ->
    case orddict:find( path, Opts ) of
        error -> % Just Returning, So give back Contents.
            {ok, Contents};
        {ok, Path} -> % Saving, so return report string.
            Save_Path = determine_path( Path, Ext ),
            (case file:write_file( Save_Path, Conv( Contents ) ) of
                ok -> "Successful save of: "++Save_Path++"\n";
                Err -> reportize("Problem saving: "++Save_Path, Err)
            end)
    end.

%% @hidden
%% @doc Returns the previous path with a new extension.
determine_path( Path, NewExt ) -> filename:rootname(Path)++NewExt.
determine_module( Path ) -> filename:rootname(filename:basename(Path)).

%% @hidden
%% @doc Wraps Erlang AbsForms with the neccessary boilerplate to be a module.
wrap_for_module( Forms, Options ) -> 
    ModuleName = list_to_atom(determine_module( orddict:fetch( path, Options ) )),
    AbsForms = [
        erl_syntax:attribute( erl_syntax:atom(module),
                                             [erl_syntax:atom(ModuleName)]),
        erl_syntax:attribute( erl_syntax:atom(export),
                                             [erl_syntax:list([
                                                 erl_syntax:arity_qualifier( 
                                                    erl_syntax:atom(main),
                                                    erl_syntax:integer(1))])]),
        build_main_fun( Forms )],
    lists:map( fun erl_syntax:revert/1, AbsForms ).


%% @hidden
%% @doc Injects the module information in front of the source.
inject_module( Erl, Options ) ->
    ModuleName = determine_module( orddict:fetch(path, Options) ),
    lists:flatten([
        "-module(", ModuleName, ").\n",
        "-export([main/1]).\n",
        Erl]).

%% @hidden
%% @doc Wrap the erlang source to match the above wrap_for_module/2. 
wrap_for_source( Erl ) ->
    lists:flatten([
       "main(Args) when is_list(Args)->\n",
        "erlam_rts:setup(Args),\n",
        "X = ", Erl, ",\n",
        "erlam_rts:breakdown(),\n",
        "io:format(\"Res: ~p~n\",[X]).\n"]).

%% @hidden
%% @doc Wraps our forms in a main function that is essentially the following:
%%      ``` main( CArgs ) -> 
%%              erlam_rts:setup(CArgs), X=Forms, erlam_rts:breakdown(), X.
%%      '''
build_main_fun( Forms ) ->
    Setup = erl_syntax:application( erl_syntax:atom( erlam_rts ),
                                    erl_syntax:atom( setup ),
                                    [erl_syntax:variable('CArgs')] ),
    Set = erl_syntax:match_expr( erl_syntax:variable('X'), 
                                 Forms ),
    BreakDown = erl_syntax:application( erl_syntax:atom( erlam_rts ),
                                        erl_syntax:atom( breakdown ),
                                        [] ),
    Return = erl_syntax:variable('X'),
    Clause = erl_syntax:clause([ erl_syntax:variable('CArgs') ], [],
                               [ Setup, Set, BreakDown, Return ] ),
    erl_syntax:function( erl_syntax:atom( main ), [ Clause ] ).

