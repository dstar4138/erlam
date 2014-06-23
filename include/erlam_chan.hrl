%%
%% The default Swap channel implementation.
%%
-record(chan,{id :: integer(), cpid :: pid(),
              mod = erlam_chan_block :: atom()}).
-type channel() :: #chan{}.
