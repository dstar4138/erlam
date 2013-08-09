% Parsed tokens.
-record(erlam_var,  {name :: atom(), value :: erlam_val()}).
-record(erlam_app,  {exp1 :: erlam_exp(), exp2 :: erlam_exp()}).
-record(erlam_if,   {exp :: erlam_exp(), texp :: erlam_exp(), fexp ::erlam_exp()}).
-record(erlam_swap, {chan :: erlam_chan(), val :: erlam_exp()}).
-record(erlam_fun,  {var :: erlam_var(), exp :: erlam_exp()}).

%% Absractions on type in case we want to add different Types.
-type erlam_var()  :: nil_var | #erlam_var{}.
-type erlam_app()  :: #erlam_app{}.
-type erlam_if()   :: #erlam_if{}.
-type erlam_swap() :: #erlam_swap{}.
-type erlam_fun()  :: #erlam_fun{}.

%% Value is the type of a veriable.
-type erlam_val() :: integer() 
                   | erlam_fun().

%% Expressions as explained in the README.
-type erlam_exp() :: integer()
                   | erlam_var()
                   | erlam_app()
                   | erlam_if()
                   | erlam_swap()
                   | erlam_spawn()
                   | erlam_fun().


