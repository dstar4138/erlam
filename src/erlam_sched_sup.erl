%%% The Scheduler Supervisor
%%%
%%%     This module handles the launching of each supervisor per logical 
%%%     processing unit or however the user-provided options declare it to be.
%%%
%%% @author Alexander Dean
-module(erlam_sched_sup).
-behaviour(supervisor).
-include("debug.hrl").

%% RTS API
-export([startup/1, shutdown/1]).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Load the schedulers into place and return the primary ID.
startup( Options ) ->
    {PrimaryID, Children} = generate_children( Options ),
    case start_link( Children ) of
        {ok, Pid} ->{ok, PrimaryID, Pid};
        ignore -> {error, rts_failure};
        {error,R} -> {error, R}
    end. 

%% @doc Shut down all children and the supervisor itself.
shutdown( Pid ) -> 
    erlam_sched:stopall(),
    exit( Pid, shutdown ).

%% @doc Start the supervisor and link it to the calling process.
start_link( Children ) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Children).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Generate the schedulers based on the provided options and then
%%   initialize them all.
%% @end  
init( Children ) ->
    process_flag( trap_exit, true ),
    {ok, {{one_for_all, 5, 10}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Generates the CHILD spec for the supervisor API. It determines
%%   this by going though defaults and 
generate_children( Options ) ->
    {Module, InitOptions} = get_sched_module( Options ),
    
    % Grab the System Topology to check against what our options are.
    Topology = erlang:system_info( cpu_topology ),
    
    % The provided scheduler module will be able to give the layout mapping of
    % the scheduler modules over the processing units. If the scheduler layout 
    % calls for multiple different schedulers, that's fine. (In otherwords, we 
    % are able to have multiple scheduler modules all working on the same 
    % system.) 
    {PrimaryID, Layout} = erlang:apply( Module, layout, [ Topology, InitOptions ] ),
    Children = layout_to_children( PrimaryID, Layout ),
    {PrimaryID, Children}.

%% @hidden
%% @doc Based on the Layout returned by the scheduler callback, we create
%%   the children accordingly.
%% @end  
layout_to_children( _, [] ) -> [];
layout_to_children( Primary, [{ID,Module,Opts}|Rest] ) ->
    [ ?CHILD(ID,erlam_sched,worker,[ID,Primary,Module,Opts]) |
      layout_to_children(Primary,Rest) 
    ].
    
%% ------------------------------------------
%% Option Parsing

%% @hidden
%% @doc Get the primary scheduler module provided by the user. If nothing is
%%   given then it falls back to the default scheduler.
%% @end   
get_sched_module( Options ) ->
    {value, {scheduler,Scheduler}, Opts} = 
                        lists:keytake( scheduler, 1, Options ),
    {value, {options, InitOpts}, _} = 
                        lists:keytake( options, 1, Opts ),
    {Scheduler, InitOpts}.

