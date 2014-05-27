-define(DEBUG_RUN, true).


-ifdef(DEBUG_RUN).
-define(DEBUG( Msg ), io:format("~p|"++Msg,[self()])).
-define(DEBUG( Msg, Format ), io:format("~p|"++Msg, [self()|Format])).

-else.
-define(DEBUG( _ ), true).
-define(DEBUG( _, _), true).

-endif.


-define(ERROR(Section, Message, Format), 
        io:format(string:concat("ERR:~p:",Message),[Section|Format])).
