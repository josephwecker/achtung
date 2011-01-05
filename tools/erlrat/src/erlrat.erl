-module(erlrat).
-export([ast_to_matchfuns/1]).


-record(attrs, {
    notp=false,
    andp=false,
    star=false,
    plus=false,
    opt =false,
    tok =false,
    drop=false
  }).

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
  Combined = [combine_rules(TL,Defs,TopLevels)||{TL,true}<-TopLevels:to_list],
  Combined.

% Create general purpose lookup dictionary for the rules
make_defs([],Defs,EPs) -> {Defs,EPs};
make_defs([{{'ENTRY',Name},TrP,TrE,Body}|R],Defs,EPs) ->
  make_defs(R,Defs:store(Name,{TrP,TrE,Body}),[Name|EPs]);
make_defs([{Name,TrP,TrE,Body}|R],Defs,EPs) ->
  make_defs(R,Defs:store(Name,{TrP,TrE,Body}),EPs).

% Add entrypoints to toplevel and scan rules for recursion (which implies they
% need to be entrypoints as well).
scan_tls(EPs,Defs) -> scan_tls(EPs,Defs,dict:new()).
scan_tls([],_Defs,Tops) -> Tops;
scan_tls([EP|R],Defs,Tops) ->
  {_TrP,_TrE,Body} = Defs:fetch(EP),
  Tops2 = Tops:store(EP,true),
  Tops3 = scan_for_tops(Defs,Body,[],Tops2),
  scan_tls(R,Defs,Tops3).

scan_for_tops(Defs,{rule,_,Name},Seen,Tops) ->
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
scan_for_tops(Defs,{_,_,Exprs},Seen,Tops) when is_list(Exprs) ->
  lists:foldl(fun(Body,OldTops) -> scan_for_tops(Defs,Body,Seen,OldTops) end,
    Tops, Exprs);
scan_for_tops(_,_,_,Tops) -> Tops.


combine_rules(TLName, Defs, TLs) ->
  {TrP,TrE,Body} = Defs:fetch(TLName),
  {TLName, match_form(Body,TrP,TrE,Defs,TLs)}.

% Returns:
% 

% any
match_form({any,Attrs},TrP,TrE,_,_) ->
  {match,{any,Attrs},{succ,TrP},{fail,TrE}};

% lit (literal strings)
match_form({lit,Attrs,Str},TrP,TrE,_,_) ->
  {match,{Str,Attrs},{succ,TrP},{fail,TrE}};

% char (list of character ranges e.g., [A-Z_])
match_form({char,Attrs,[]},TrP,TrE,_,_) -> {fail,TrE};
match_form({char,Attrs,[Range|R]},TrP,TrE,_,_) ->
  {match,{Range,Attrs},{succ,TrP},
    match_form({char,[],R},TrP,TrE,_,_)};

% ord (ordered choices)
match_form({ord,Attrs,[]},TrP,TrE,Defs,TLs) -> fail;
match_form({ord,Attrs,[Choice|R]},TrP,TrE,Defs,TLs) ->





