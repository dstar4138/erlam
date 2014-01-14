%% ErLam Utility Functionality.
%%
%%  Functions found in the standard library or utilized throughout the compiler 
%%  itself.
%%
-module(erlam_util).

%% Std Library.
-export([church/1, unchurch/1, hang/1]).

%%% ==========================================================================
%%% Standard Library Functionality
%%% ==========================================================================

%% @doc Church Numeral generation given an integer. This is called from the 
%%   built-in function 'rep' in the std library.
%% @end  
church( 0 ) -> 
    fun(_F) -> % c(0) = \f -> id
        fun(X) -> X end 
    end;
church( N ) when is_integer(N) andalso N > 0 -> 
    fun(F) -> 
        fun(X) -> % c(n) = \f -> \x -> f (c(n-1) f x)
            Next = church(N-1),
            App = erlang:apply( Next, [F] ),
            Val = erlang:apply( App, [X] ), 
            erlang:apply(F, [Val])
        end 
    end.


%% @doc Church Numeral conversion given a function. This create an integer
%%   from a function (as long as it's defined correctly. This is called from
%%   the built-in function 'unrep' in the std library.
%% @end  
unchurch( F ) -> 
    IdPlus = fun(X) -> X+1 end,
    Xf = erlang:apply( F, [ IdPlus ] ),
    erlang:apply( Xf, [ 0 ] ).


%% @doc Put the process to sleep in the scheduler for at least X seconds.
hang( X ) ->
    Seconds = timer:seconds( X ),
    timer:sleep( Seconds ), % TODO: This should modify the current RTS.
    1.% Returns a valid Expression.

