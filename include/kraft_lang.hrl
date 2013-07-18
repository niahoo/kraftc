-record(kraftmod,  { filename, parsetree , forms, beam, signatures , name, step}).

-type line() :: integer.
-type quantity() :: integer() | service.
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

-type signature() :: {TechnicName :: atom(), [signatureclause()]}.
-type signatureclause() :: {InputInfo :: [{TypeName :: atom(), quantity()}] , OutputInfo :: [OutputPossibleList :: [TypeName :: atom()]]}.


