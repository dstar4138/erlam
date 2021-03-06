-define(DEBUG_RUN, true).


-ifdef(DEBUG_RUN).
-define(DEBUG( Msg ), io:format("~p@~p|"++Msg,[self(),element(3,erlang:now())])).
-define(DEBUG( Msg, Format ), io:format("~p@~p|"++Msg, [self(),element(3,erlang:now())|Format])).
%-define(DEBUG( Msg ), error_logger:info_msg("~p|"++Msg,[self()])).
%-define(DEBUG( Msg, Format ), error_logger:info_msg("~p|"++Msg, [self()|Format])).

-else.
-define(DEBUG( _X ), true).
-define(DEBUG( _X, _Y), true).

-endif.


-define(ERROR(Section, Message, Format), 
        io:format(string:concat("ERR:~p:",Message),[Section|Format])).
