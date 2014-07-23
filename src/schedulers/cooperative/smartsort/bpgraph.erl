%%% 
%%% Fast Queue-like Structure on Bi-Partite Graphs
%%%
%%% Note: it has been optimized for the purpose of a process queue. 
%%% Note: We assume particular stdlib implementations for fast operations.
-module(bpgraph).

%% Requires process introspection for pruning and comparisons
-include("process.hrl").

% Process Queue operations
-export([new/0, new/1, len/1, is_empty/1, in/2, out/1]).

% Graph Functionality
-export([resort/1, update_edge/4, pop_by_sigma/3, popular_sigma/1]).

%% Internal Graph state Representation:
%%  Graph is essentially two sets Rho(process) -> Sigma(channel)
-record(bpgraph, {
    rho = queue:new(), % User stdlib fast queue implementation
    sortfun = fun default_sort/3, % User defined sort function on the rho set.
    edges = orddict:new(), % [Ref_{rho_i} -> [{Sigma_i, EdgeVal}] ]
    sigma = orddict:new(), % [Sigma_i -> [Ref_{rho_i}] key orderings
    stolen = ordsets:new() % Keep track of stolen Rho_i for queue prunning
}).

%%% ==========================================================================
%%% Queue Operations
%%% ==========================================================================

%% @doc Create a new FQBPG. O(1)
new() -> #bpgraph{}.
new( SortFun ) when is_function( SortFun, 3 ) -> #bpgraph{sortfun=SortFun}.

%% @doc Checks the length of rho. O( P )
len( #bpgraph{rho=P} ) -> queue:len( P ).

%% @doc Checks to make sure rho is not empty. O(1) 
is_empty( #bpgraph{rho=P} ) -> queue:is_empty( P ).

%% @doc Inserts the item back into the queue. O(1)
in( Pi, #bpgraph{rho=P}=G ) -> G#bpgraph{rho=queue:in(Pi, P)}.

%% @doc Extracts an item from the queue, ignores sigma. 
%%      O(1) amortized, O(P) worst case.
out( #bpgraph{rho=P, stolen=S}=G ) ->
    case queue:out(P) of
        {empty,_} -> {empty, G};
        {{value, Res},NP}  ->
            Ref_rhoi = Res#process.proc_id, %Check if process is on stolen list
            NG = G#bpgraph{rho=NP},
            case ordsets_prune( Ref_rhoi, S ) of
                {true, NS} -> out( NG#bpgraph{stolen=NS} );
                false -> {{value, Res}, NG}
            end
    end.

%%% ==========================================================================
%%% Graph Functionality
%%% ==========================================================================
%% Given that P is the length of the Rho set, and S is the length of the sigma
%% set, we define the following functions: 
%%        resort -> O( 2P + P log P ) assuming lists:sort/1 is mergesort.
%%   update_edge -> O( S ) amortized, O( PS ) worst case.
%%  pop_by_sigma -> O( PS )
%%

%% @private
%% @doc Default Sorting function called by resort/1 if new/0 was used. Sort
%%   function returns a value for the particular Rho which gives the process
%%   queue a way to sort it. By default, we sort based on recency of 
%%   the synchronizations.
%% @end
default_sort( _Rho_i, EdgeSet, SigmaSet ) ->
    SumOfEdges = lists:foldl(fun(E,A) -> E+A end, 0, EdgeSet),
    SumOfEdges / (length(SigmaSet)+1).

%% @doc Sort the rho set based on a user-defined sort-function. The sort 
%%   function takes three values, the particular Rho, the set of edges and
%%   the set of Sigmas that they are connected to in the same order. The 
%%   sort function should return a comparable erlang term which will be
%%   passed to lists:sort/1 for shuffling.
%% @end
resort( #bpgraph{rho=P, sortfun=F, edges=Es}=G ) ->
    Rhos = queue:to_list(P),
    RhoList = lists:foldl( 
        fun( Rho_i, Counters ) ->
            Ref_rhoi = Rho_i#process.proc_id,
            Edges = orddict_get_value( Ref_rhoi, Es, [] ),
            {Sigmas, EdgeVals} = lists:unzip( Edges ),
            RhoiVal = case catch F( Rho_i, EdgeVals, Sigmas ) of
                          {'EXIT',_} -> 0;
                          V -> V
                      end,
            [ { RhoiVal, Rho_i } | Counters ]
        end, [], Rhos ),
    {_, Sorted} = lists:unzip( lists:sort( RhoList ) ),
    G#bpgraph{rho=queue:from_list(Sorted)}.

%% @doc Add or Update an edge value between the Rho set (process queue) and the
%%   Sigma set (channels).
%% @end
update_edge( Rho, Sigma, Val, #bpgraph{ edges=Es, sigma=S }=G ) ->
    Ref_rhoi = Rho#process.proc_id,
    NewSigma = orddict_add_value( Sigma, Ref_rhoi, S ),
    NewEdges = orddict_multimap_update( Ref_rhoi, Sigma, Val, Es ),
    G#bpgraph{ edges=NewEdges, sigma=NewSigma }.

%% @doc Pop a percentage of the rhos which have a sigma in common, could be
%%   converted to lock-free.
%% @end
pop_by_sigma( Sigma, Threshold, #bpgraph{ sigma=S, rho=P, stolen=T }=G ) 
                            when Threshold > 0.0 andalso Threshold =< 1.0 ->
    RhoRefs = orddict_get_value( Sigma, S, [] ),
    StealCount = erlang:round( length(RhoRefs) * Threshold ),
    {RhoIds, NewRhoRefs} = lists:split(StealCount, RhoRefs),
    Rhos = queue_get_by_reference( RhoIds, queue:to_list(P) ), 
    {Rhos, G#bpgraph{sigma=orddict:store( Sigma, NewRhoRefs, S),
                     stolen=T++RhoIds}}.

%% @doc Get a set of the most popular channel's for this scheduler
%%   currently. We do this so as to steal from another scheduler with this
%%   channel.
%$ @end
popular_sigma( #bpgraph{ sigma=S } ) ->
    Degree = fun( {Sigma_i, Rhos} ) -> {length(Rhos), Sigma_i} end,
    {_, OrderedSigma} = lists:unzip( 
                            lists:sort( 
                                lists:map( Degree, orddict:to_list(S)))),
    {ok, OrderedSigma}.


%%% ==========================================================================
%%% STDLIB Extended Functionality
%%% ==========================================================================

%% @doc A del_element with the added acknowledgment of existence in the set.
ordsets_prune( E, Set ) -> ordsets_prune( E, Set, [] ).
ordsets_prune( E, [H|Es], R ) when E > H -> ordsets_prune(E,Es,[H|R]);
ordsets_prune( E, [H|_], _  ) when E < H -> false;
ordsets_prune( _, [_|Es], R ) -> {true, lists:rev(R)++Es}; % E==H
ordsets_prune( _, [], _ ) -> false.

%% @doc Lookup a value in an orddict and return a default value.
orddict_get_value( E, Dict, Default ) ->
    case orddict:find( E, Dict ) of
        error -> Default;
        {ok,Value} -> Value
    end.

%% @doc Updates a value in an ordered dictionary by appending a value to the
%%   Key's ordset value.
%% @end
orddict_add_value( Key, Value, [{OKey,_}=Pair|Dict] ) when Key > OKey -> 
    [Pair | orddict_add_value( Key, Value, Dict )];
orddict_add_value( Key, Value, [{OKey,_}=Pair|Dict] ) when Key < OKey -> 
    [{Key, [Value]}, Pair | Dict];
orddict_add_value( Key, Value, [{Key,Set}|Dict] ) ->
    NewSet = ordsets:add_element( Value, Set ),
    [{Key, NewSet} | Dict];
orddict_add_value( Key, Value, _ ) ->
    [{Key, [Value]}].

%% @doc When using the ordered dictionary as a table, we need a fast way to
%%   access and update a value internally.
%% @end
orddict_multimap_update( Key1, Key2, Value, [{Key,_}=Pair|Dict] ) 
    when Key1 > Key -> [Pair | orddict_multimap_update( Key1, Key2, Value, Dict )];
orddict_multimap_update( Key1, Key2, Value, [{Key,_}=Pair|Dict] ) 
    when Key1 < Key -> [{Key1,[{Key2,Value}]}, Pair | Dict];
orddict_multimap_update( Key1, Key2, Value, [{Key1,Row}|Dict] ) ->
    [{Key1,orddict:store(Key2,Value,Row)}|Dict];
orddict_multimap_update( Key1, Key2, Value, _ ) ->
    [{Key1,[{Key2,Value}]}].

%% @doc Get a set of Processes from a list based on its proc_id.
queue_get_by_reference( [], _ ) -> [];
queue_get_by_reference( [H|T], Ps ) ->
    case find( H, Ps ) of
       {ok, V} -> [V|queue_get_by_reference( T, Ps )];
        _ -> queue_get_by_reference(T, Ps)
    end.
find( _, [] ) -> false;
find( H, [#process{proc_id=H}=P|_] ) -> {ok, P};
find( H, [_|T] )-> find( H, T ).

