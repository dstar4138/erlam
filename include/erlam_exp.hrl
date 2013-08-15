% Parsed tokens.
-record(erlam_var,  {name :: atom()}).
-record(erlam_app,  {exp1 :: erlam_exp(), exp2 :: erlam_exp()}).
-record(erlam_if,   {exp :: erlam_exp(), texp :: erlam_exp(), fexp ::erlam_exp()}).
-record(erlam_swap, {chan :: erlam_chanvar(), val :: erlam_exp()}).
-record(erlam_spawn,{exp :: erlam_exp()}).
-record(erlam_fun,  {var :: erlam_var(), exp :: erlam_exp()}).
-record(erlam_chan, {chan :: integer()}).

%% Absractions on type in case we want to add different Types.
-type erlam_var()  :: nil_var | #erlam_var{}.
-type erlam_app()  :: #erlam_app{}.
-type erlam_if()   :: #erlam_if{}.
-type erlam_swap() :: #erlam_swap{}.
-type erlam_spawn():: #erlam_spawn{}.
-type erlam_fun()  :: #erlam_fun{}.
-type erlam_chan() :: #erlam_chan{}.
-type erlam_newchan() :: newchan.

%% The Actual Channel reference or a variable to look it up with.
-type erlam_chanvar() :: erlam_chan() 
                       | erlam_var().

%% Value is the type of a variable.
-type erlam_val() :: integer() 
                   | erlam_fun()
                   | erlam_chan().

%% Expressions as explained in the README.
-type erlam_exp() :: integer()
                   | erlam_var()
                   | erlam_app()
                   | erlam_if()
                   | erlam_swap()
                   | erlam_spawn()
                   | erlam_fun()
                   | erlam_newchan().

% Special Erlang-Code expressions
-record(erlam_erl, {arity :: integer(), func :: string()}).
-type erlam_erl() :: #erlam_erl{}.
-type erlam_ast() :: erlam_exp() | erlam_erl().
