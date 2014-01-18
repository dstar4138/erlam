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
    Line = case orddict:find( line, Options ) of
        {ok, L} -> L; _ -> 0
    end,
    case erlam_trans:to_forms( AST, Line ) of
        {ok, Forms} -> save_or_return( Forms, fun t2s/1, ".forms", Options );
        Err -> reportize( "Compiling Abstract Forms Failed", Err )
    end.

%% @doc Takes an internal AST and converts it to raw Erlang Source code. If the
%%   option `path' is present, then it will save the forms to a new file along-
%%   side the file in `path'.
%% @end  
to_erl( AST, Options ) ->
    Source = erlam_trans:to_erl( AST ),
    ID = fun(X)-> wrap_for_source(X, Options) end,
    save_or_return(Source, ID, ".erl", Options). 

%% @doc Takes an internal AST and converts it to an Erlang BEAM Module. If the
%%   option `path' is present, then it will save the forms to a new file along-
%%   side the file in `path'.
%% @end  
to_beam( AST, Options ) ->
    {ok, Forms} = to_forms( AST, [] ),
    ID = fun(_X)-> % Intentional Ignore, we don't care about the partial BEAM.
            compile:forms( wrap_for_module(Forms, Options) )
         end, 
    case compile:forms( Forms ) of
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
    Source = wrap_for_source( Erl, Options ),
    Sections = [shebang, comment, {emu_args, "+nowarn_unused_vars +nowarn_shadow_vars -pa "++?EBIN_DIR}, 
                {source, list_to_binary(Source)}],
    case orddict:find( path, Options ) of
        error -> % Just Returning, So give back Contents.
            {ok, Source};
        {ok, Path} -> % Saving
            Save = determine_path( Path, "" ),
            escript:create( Save, Sections ),
            "Successful save of "++Save++"\n"
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
reportize( Msg, {error, E} ) -> io_lib:format("Error: ~s -> ~p~n",[Msg,E]).

%% @hidden
%% @doc Push all lines of the report to the terminal.
display_report( [], _ ) -> ok;
display_report([L|R], Opts) -> % TODO: Check verbosity options
    io:put_chars(L), display_report(R,Opts).

%% @hidden
%% @doc Based on the options we can generate a BEAM, Erl Source, Erl Abstract
%%   Forms, and a standalone Escript.
%% @end  
push_to_files( AST, Options ) ->
  check_then_run( to_forms, Options, AST ),
  check_then_run( to_erl, Options, AST ),
  check_then_run( to_beam, Options, AST ),
  check_then_run( to_escript, Options, AST).

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
        {ok, Path} -> % Saving
            Save_Path = determine_path( Path, Ext ),
            (case file:write_file( Save_Path, Conv( Contents ) ) of
                ok -> "Successful save of: "++Save_Path;
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
    ModuleName = determine_module( orddict:fetch( path, Options ) ),
    FileName = ModuleName ++".erl",
    [{attribute,1,file,{FileName,1}}, % Arbitrary Line numbers, this is bad.
     {attribute,1,module,ModuleName},
     {attribute,2,export,[{main,1}]},
     build_main_fun( Forms ),
     {eof,19}].

%% @hidden
%% @doc Wrap the erlang source to match the above wrap_for_module/2. 
wrap_for_source( Erl, _Options ) ->
    lists:flatten([
        "main(Args) when is_list(Args)->",
        "erlam_rts:setup(Args),",
        "X = ", Erl, ",",
        "erlam_rts:breakdown(),",
        "io:format(\"Res: ~p~n\",[X])."]).

%% @hidden
%% @doc Wraps our forms in a main function that is essentially the following:
%%      ``` main( CArgs ) -> 
%%              erlam_rts:setup(CArgs), X=Forms, erlam_rts:breakdown(), X.
%%      '''
build_main_fun( Forms ) ->
    Line = 1, % TODO: Find a way to make this dynamic.
    {function,Line,main,1,[{clause,Line,[{var,Line,'CArgs'}],[],[
        {call,Line,{remote,Line,{atom,Line,erlam_rts},
                                {atom,Line,setup}},
                                [{var,Line,'CArgs'}]},
        {match,Line, {var,Line,'X'}, Forms},
        {call,Line,{remote,Line,{atom,Line,erlam_rts},{atom,Line,breakdown}},[]},
        {call,Line,{remote,Line,{atom,Line,io},{atom,Line,format}},[
                        {string,Line,"Res: ~p~n"},{cons,Line,{var,Line,'X'},
                                                             {nil,Line}}]}]}]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%push_code( Type, Path, UserCode ) ->
%    {ok, BasePath, ModName} = get_path_info( Path ),
%    {ok, FilePath, _Source} = build_user_module( Type, BasePath, ModName, UserCode ),
%    build_executable( FilePath ).
%
%get_path_info( Path ) ->
%    BasePath = filename:dirname( Path ),
%    ModName  = strip_ext(filename:basename( Path )),
%    {ok, BasePath, ModName}.
%strip_ext( Path ) -> lists:takewhile( fun (C) -> C /= $. end, Path ).
%
%% Creates the file ModName.erl where ModName.els was the source file
%% used when called compile_file/1.
%build_user_module( Type, BasePath, ModName, UserCode ) ->
%    FilePath = string:join([BasePath,ModName],"/"),
%    Source = wrap_user_code(Type, ModName, UserCode),
%    case Type of erlang ->
%                    file:write_file(string:concat(FilePath,".erl"),Source);
%                 escript ->
%                    file:write_file(FilePath,Source)
%    end,            
%    {ok, FilePath, Source}.
%        
%% Combines the rts,chan, and UserCode modules together into an archive escript.
%build_executable(_Path) -> ok.
%
%wrap_user_code( erlang, Module, Code ) ->
%    io_lib:format( 
%        string:join(["-module(~s).",
%                     "-export([main/1]).",
%                     "main(Args) when is_list(Args)->",
%                     "erlam_rts:setup(Args),",
%                     "X = ~s,",
%                     "erlam_rts:breakdown(),",
%                     "X."],"~n"),
%                  [Module,Code]);
%
%wrap_user_code( escript, Module, Code ) ->
%    io_lib:format(
%        string:join(["#!/usr/bin/env escript",
%                     "%% -*- erlang -*-",
%                     "%%! -smp enable -sname ~s",
%                     "main(Args) ->",
%                     "erlam_rts:setup(Args),",
%                     "X = ~s,",
%                     "erlam_rts:breakdown(),",
%                     "X."],"~n"),
%        [Module, Code]).
%
%getType( _Configs ) -> escript.
%
