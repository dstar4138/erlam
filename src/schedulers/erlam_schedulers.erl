%%% 
%%% Scheduler Listings module
%%%     Can quite easily be replaced by introspection of compilation
%%%     directory, but this is easier for the time being.
%%%
-module(erlam_schedulers).

%% Public functionality.
-export([ list/0 ]).
-export([ verify/1, get_options/1 ]).

%% @doc Returns a list of the default scheduler modules and their simple
%%   description.
%% @end
-spec list() -> [{atom(), iodata()}].
list() ->
    [ {erlam_sched_single, "Single-core Round-Robin Scheduler"},
      {erlam_sched_global, "Multi-core Round-Robin Shared-Queue Scheduler"},
      {erlam_sched_single_cml, "Single-core Dual-Queue Scheduler"},
      {erlam_sched_multi_ws, "Multi-core Work-Stealing Round-Robin Scheduler"}
    ].

%% @doc Verify and check if the given scheduler name is an actual scheduler.
-spec verify( iodata() ) -> {ok, atom()} 
                          | {error, {missing, atom(), integer()}}
                          | {error, any()}.
verify( SchedName ) ->
    ModuleName = list_to_atom( SchedName ), 
    case code:ensure_loaded( ModuleName ) of
        {module,_} -> check_sched_behaviour( ModuleName );
        {error,_ } -> {error, badarg}
    end.

%% @doc Query the scheduler module for an optional function which will return
%%   a printout of the arguments the function takes and will parse.
%% @end
-spec get_options( atom() ) -> {ok, string()} | false.
get_options( SchedModule ) -> 
    case 
        catch erlang:apply( SchedModule, options, [] ) 
    of
        {ok, Options } -> {ok, Options};
        _ -> false
    end.

%%% ==========================================================================
%%% Internal Functionality
%%% ==========================================================================

%% @hidden
%% @doc Check a loaded module for all required scheduler functions.
%% @see erlam_scheduler:required/0.
check_sched_behaviour( ModuleName ) ->
    ModMeta = erlang:apply( ModuleName, module_info, [] ),
    {exports, Exports} = lists:keyfind( exports, 1, ModMeta ),
    Required = erlam_scheduler:required(),
    check_if_funs_exist( Exports, Required, ModuleName ).
check_if_funs_exist(_, [], Mod) -> {ok, Mod};
check_if_funs_exist( [], [{Missing,Arity}|_], _ ) ->
    {error,{missing, Missing, Arity}};
check_if_funs_exist( [Fun|Rest], Required, Mod) ->
     check_if_funs_exist( Rest, check_remove( Fun, Required ), Mod ).
check_remove( _, [] ) -> [];
check_remove( {Name, Arity}, [{Name, Arity}|Rest] ) -> Rest;
check_remove( V, [H|R] ) -> [H|check_remove(V,R)].

