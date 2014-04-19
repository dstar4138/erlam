-define(DEBUG_RUN, true).


-ifdef(DEBUG_RUN).
-define(DEBUG( Msg ), io:format(Msg,[])).
-define(DEBUG( Msg, Format ), io:format(Msg, Format)).

-else.
-define(DEBUG( _ ), true).
-define(DEBUG( _, _), true).

-endif.


-define(ERROR(Section, Message, Format), 
        io:format(string:concat("ERR:~p:",Message),[Section|Format])).
