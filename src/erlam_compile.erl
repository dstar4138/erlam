%% 
%% ErLam Compiler Driver
%%  Drives the compilation process. Will provide hooks for dumping intermediate
%%  representations for review.
%%
-module(erlam_compile).
-export([compile_file/1, compile_file/2]).


compile_file( Path ) -> compile_file( Path, [] ).
compile_file( Path, Configs ) -> %TODO: Ignoring comfigurations for now.
    %TODO: NO ERROR HANDLING?!
    {ok, BinaryContents} = file:read_file( Path ),
    StringContents = erlang:binary_to_list(BinaryContents),
    {ok, Tokens, _EndLine} = erlam_lexer:string( StringContents ),
    {ok, ExprTree} = erlam_parser:parse( Tokens ),
    AST = erlam_lib:update( ExprTree ), 
    UserCode = erlam_trans:to_erl(AST),
    push_code( getType(Configs), Path, UserCode ).


push_code( Type, Path, UserCode ) ->
    {ok, BasePath, ModName} = get_path_info( Path ),
    {ok, FilePath, _Source} = build_user_module( Type, BasePath, ModName, UserCode ),
    build_executable( FilePath ).

get_path_info( Path ) ->
    BasePath = filename:dirname( Path ),
    ModName  = strip_ext(filename:basename( Path )),
    {ok, BasePath, ModName}.
strip_ext( Path ) -> lists:takewhile( fun (C) -> C /= $. end, Path ).

% Creates the file ModName.erl where ModName.els was the source file
% used when called compile_file/1.
build_user_module( Type, BasePath, ModName, UserCode ) ->
    FilePath = string:join([BasePath,ModName],"/"),
    Source = wrap_user_code(Type, ModName, UserCode),
    case Type of erlang ->
                    file:write_file(string:concat(FilePath,".erl"),Source);
                 escript ->
                    file:write_file(FilePath,Source)
    end,            
    {ok, FilePath, Source}.
        
% Combines the rts,chan, and UserCode modules together into an archive escript.
build_executable(_Path) -> ok.

wrap_user_code( erlang, Module, Code ) ->
    io_lib:format( 
        string:join(["-module(~s).",
                     "-export([main/1]).",
                     "main(Args) when is_list(Args)->",
                     "erlam_rts:setup(Args),",
                     "X = ~s,",
                     "erlam_rts:breakdown(),",
                     "X."],"~n"),
                  [Module,Code]);

wrap_user_code( escript, Module, Code ) ->
    io_lib:format(
        string:join(["#!/usr/bin/env escript",
                     "%% -*- erlang -*-",
                     "%%! -smp enable -sname ~s",
                     "main(Args) ->",
                     "erlam_rts:setup(Args),",
                     "X = ~s,",
                     "erlam_rts:breakdown(),",
                     "X."],"~n"),
        [Module, Code]).

getType( _Configs ) -> escript.

