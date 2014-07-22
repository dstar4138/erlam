%% An Dummy Module to reference 
%%      ./test.ex -s erlam_sched_multi_ws -- shared_queue
%% without needing the parameter change on the scheduler.
%%
-module(erlam_sched_multi_ws_sq).
-behaviour(erlam_scheduler).

%% ErLam Scheduler API
-export([ layout/2, init/1, cleanup/1, tick/2, spawn_process/2 ]).
-export([ options/0 ]).

layout( Top, Opt ) -> erlam_sched_multi_ws:layout( Top, Opt ).
init( Opt ) -> erlam_sched_multi_ws:init( [{shared_queue,true}|Opt] ).
cleanup( State ) -> erlam_sched_multi_ws:cleanup( State ).
tick( Status, State ) -> erlam_sched_multi_ws:tick( Status, State ).
spawn_process( P, State ) -> erlam_sched_multi_ws:spawn_process( P, State ).
options() -> erlam_sched_multi_ws:options().
