-module(json_sample).
-compile(export_all).

sample_peg_grammar() -> "
     >json_value <- s? (object/array/string/number/true/false/null) s?
     object      <- '{' s? pair (s? ',' s? pair)* s? '}'
                  / '{' s? '}' `object()`
     pair        <- s? string s? ':' s? json_value s? `pair()`
     >array      <- '[' s? json_value (s? ',' s? json_value)* s? ']'
                  / '[' s? ']' `array()`
     string      <- '\"' (!'\"' ('\\\\\\\\' / '\\\\\"' / .))* '\"' `string()`
     number      <- int frac? exp? `number()`;
     int         <- '-'? (nz_digit digit+) / digit
     frac        <- '.' digit+
     exp         <- e digit+
     e           <- [eE] ('+' / '-')?
     nz_digit    <- [1-9]
     digit       <- [0-9]
     true        <- 'true' `true`
     false       <- 'false' `false`
     null        <- 'null' `null`
     s           <- [ \\t\\n\\r]*
  ".

sample_peg_ast() ->
  % >json_value <- s? (object/array/string/number/true/false/null) s?
  %[{{'ENTRY','json_value'},[],[],
  [{'json_value',[],[],
     {seq,[],[{rule,[opt],'s'},
              {ord,[],[{rule,[],'object'},
                       {rule,[],'array'},
                       {rule,[],'string'},
                       {rule,[],'number'},
                       {rule,[],'true'},
                       {rule,[],'false'},
                       {rule,[],'null'}]},
              {rule,[opt],'s'}]}},
  % object      <- '{' s? pair (s? ',' s? pair)* s? '}'
  %              / '{' s? '}' `object()`
   {'object',"object()",[],
     {ord,[],[{seq,[],[{lit,[],"{"},
                       {rule,[opt],'s'},
                       {rule,[],'pair'},
                       {seq,[star],[{rule,[opt],'s'},
                                    {lit,[],","},
                                    {rule,[opt],'s'},
                                    {rule,[],'pair'}]},
                       {rule,[opt],'s'},
                       {lit,[],"}"}]},
              {seq,[],[{lit,[],"{"},
                       {rule,[opt],'s'},
                       {lit,[],"}"}]}]}},
  % pair        <- s? string s? ':' s? json_value s? `pair()`
   {'pair',"pair()",[],
     {seq,[],[{rule,[opt],'s'},
              {rule,[],'string'},
              {rule,[opt],'s'},
              {lit,[],":"},
              {rule,[opt],'s'},
              {rule,[],'json_value'},
              {rule,[opt],'s'}]}},
  % >array      <- '[' s? json_value (s? ',' s? json_value)* s? ']'
  %              / '[' s? ']' `array()`
   {{'ENTRY','array'},"array()",[],
     {opt,[],[{seq,[],[{lit,[],"["},
                       {rule,[opt],'s'},
                       {rule,[],json_value},
                       {seq,[star],[{rule,[opt],'s'},
                                    {lit,[],","},
                                    {rule,[opt],'s'},
                                    {rule,[],'json_value'}]},
                       {rule,[opt],'s'},
                       {lit,[],"]"}]},
              {seq,[],[{lit,[],"["},
                       {rule,[opt],'s'},
                       {lit,[],"]"}]}]}},
  % string      <- '"' (!'"' ("\\\\" / '\\"' / .))* '"' `string()`
   {'string',"string()",[],
     {seq,[],[{lit,[],"\""},
              {seq,[star],[{lit,[notp],"\""},
                           {ord,[],[{lit,[],"\\\\"},
                                    {lit,[],"\\\""},
                                    {any,[]}]}]},
              {lit,[],"\""}]}},
  % number      <- int frac? exp? `number()`;
   {'number',"number()",[],
     {seq,[],[{rule,[],'int'},
              {rule,[opt],'frac'},
              {rule,[opt],'exp'}]}},
  % int         <- '-'? (nz_digit digit+) / digit
   {'int',[],[],
     {ord,[],[{seq,[],[{lit,[opt],"-"},
                       {seq,[],[{rule,[],'nz_digit'},
                                {rule,[plus],'digit'}]}]},
              {rule,[],'digit'}]}},
  % frac        <- '.' digit+
   {'frac',[],[],
     {seq,[],[{lit,[],"."},
              {rule,[plus],'digit'}]}},
  % exp         <- e digit+
   {'exp',[],[],
     {seq,[],[{rule,[],'e'},
              {rule,[plus],'digit'}]}},
  % e           <- [eE] ('+' / '-')?
   {'e',[],[],
     {seq,[],[{char,[],[$e,$E]},
              {ord,[opt],[{lit,[],"+"},
                          {lit,[],"-"}]}]}},
  % nz_digit    <- [1-9]
   {'nz_digit',[],[],
     {char,[],[{$1,$9}]}},
  % digit       <- [0-9]
   {'digit',[],[],
     {char,[],[{$0,$9}]}},
  % true        <- 'true' `true`
   {'true',{term,true},[],
     {lit,[],"true"}},
  % false       <- 'false' `false`
   {'false',{term,false},[],
     {lit,[],"false"}},
  % null        <- 'null' `null`
   {'null',{term,null},[],
     {lit,[],"null"}},
  % s           <- [ \t\n\r]*
   {'s',[],[],
     {char,[star],[$ ,$\t,$\n,$\r]}}]
  .


                           
