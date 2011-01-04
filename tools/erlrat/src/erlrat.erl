-module(erlrat3).
-export([ast_to_matchfuns/1]).

ast_to_matchfuns([]) ->
  error_logger:error("Empty grammar- parse it yourself.",[]);
ast_to_matchfuns(AST) ->
  % Turn it into a dictionary
  {Defs, EntryPoints} = make_defs(AST, dict:new(), []),
  % Make sure there's at least one entry-point
  EPs = case EntryPoints of []->[{N,_,_,_}|_]=AST,[N];_->EntryPoints end,
  % Scan for recursive rules & entry-points
  TopLevels = scan_tls(EPs, Defs),
  % Take toplevels and build full versions
  TopLevels:to_list().


make_defs([],Defs,EPs) -> {Defs,EPs};
make_defs([{{'ENTRY',Name},TrP,TrE,Body}|R],Defs,EPs) ->
  make_defs(R,Defs:store(Name,{TrP,TrE,Body}),[Name|EPs]);
make_defs([{Name,TrP,TrE,Body}|R],Defs,EPs) ->
  make_defs(R,Defs:store(Name,{TrP,TrE,Body}),EPs).


scan_tls(EPs,Defs) -> scan_tls(EPs,Defs,dict:new()).
scan_tls([],_Defs,Tops) -> Tops;
scan_tls([EP|R],Defs,Tops) ->
  {_TrP,_TrE,Body} = Defs:fetch(EP),
  Tops2 = Tops:store(EP,true),
  io:format("Trying ~p~n", [Body]),
  Tops3 = scan_for_tops(Defs,Body,[],Tops2),
  scan_tls(R,Defs,Tops3).

scan_for_tops(Defs,{rule,_,Name},Seen,Tops) ->
  io:format("Checking out rule ~p~n", [Name]),
  case dict:is_key(Name,Tops) of
    true -> Tops;
    false -> case lists:member(Name,Seen) of
        true -> Tops:store(Name,true);
        false ->
          case Defs:find(Name) of
            error -> error_logger:error("Couldn't find a definition for ~p",[Name]);
            {ok, {_,_,Body}} -> scan_for_tops(Defs,Body,[Name|Seen],Tops)
          end
      end
  end;
scan_for_tops(Defs,{_,_,[Exprs]},Seen,Tops) ->
  lists:foldl(fun(Body,OldTops) -> scan_for_tops(Defs,Body,Seen,OldTops) end,
    Tops, Exprs);
scan_for_tops(_,_,_,Tops) -> Tops.





