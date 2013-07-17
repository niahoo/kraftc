-record(kraftmod,  { parsetree , forms, signatures , name, step}).

-type line() :: integer.
-type technicdef() :: { technicdef
                      , {name, line(), Name :: atom()}
                      , Inputs :: [typeinput()]
                      , Meta :: [metadef()]
                      , Body :: term()
                      }.

-type typeinput() :: { typeinput
                     , {typename,line(), Name :: atom() }
                     }.
-type metadef() :: [{{name,line,atom()},term()}].
