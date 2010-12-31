-module(ungbar_postprocess).

-export([forms/3]).

-include("../include/ungbar.hrl").

% Usage examples for error messages
-define(EXPORT_EXAMPLE,
  "  # Exports all versions of my_function\n"
  "@export my_function\n"
  "  # Only my_function with arity 2 and all versions of another_function\n"
  "@export my_function/2 another_function").

forms(AST, LastPos, F) ->
  %AST2 = append_do_call(AST),
  {Attrs, Funs} = lists:partition(fun(V)->element(1,V) == attribute end, lists:flatten(AST)),
  {Funs2, Avail, Exports, _Inlines} = grouped_funs(Funs),
  Attrs2 = fix_exports(Attrs, Exports, Avail),
  AST2 = Attrs2 ++ Funs2,
  % Look for module name, create one if necessary, and put it on top of the forms.
  {ModuleName, ModuleAttrForm, AST3} =
    case fixup_module(AST2,[]) of
      {none, none, PF} ->
        Nm = list_to_atom(filename:rootname(filename:basename(F))),
        {Nm, {attribute,1,module,Nm}, PF};
      {Nm, MF, PF} -> {Nm, MF, PF}
    end,
  AST4 = lists:append([[{attribute, 1, file, {F, 1}}],
                           [ModuleAttrForm],
                           AST3,
                           [{eof,LastPos}]]),
  {AST4, ModuleName}.



% Takes the arbitrarily grouped functions and groups them in a way that erlang
% expects them- by name/arity.
grouped_funs(Funs) -> grouped_funs(Funs, [], [], [], []).
grouped_funs([],FunsAcc,AvailAcc,ExportAcc,InlineAcc)->{?rev(FunsAcc),AvailAcc,ExportAcc,InlineAcc};
grouped_funs([{_,FirstPos,AN,AA,_} | _] = Remaining, FunsAcc, AvailAcc, _ExportAcc, _InlineAcc) ->
  {FunGroup, Remaining2} = lists:partition(
    fun({_,_,BN,BA,_})-> {?v_atom(AN),AA}=={?v_atom(BN),BA} end,
    Remaining),
  Clauses = [C || {_,_,_,_,C} <- FunGroup],
  Fun = {function, FirstPos, ?v_atom(AN), AA, Clauses},
  grouped_funs(Remaining2, [Fun | FunsAcc], [{?v_atom(AN),AA}|AvailAcc],[],[]).

% Takes implied exports and explicit export attributes and puts them into
% a single export attribute that's in the format Erlang loves.
fix_exports(Attrs, Exports, Avail) ->
  {Exps, Attrs2} = find_exports(Attrs, [], []),
  AllExports = Exps ++ Exports,
  ExpandedExports = expanded_exports(AllExports, Avail),
  ExpandedExports ++ Attrs2.

% Extract export statements and separate them out (for line-number on errors)
find_exports([],Found,Attr) -> {?rev(Found), ?rev(Attr)};
find_exports([{attribute, Pos, export, L}|R], Found, Attr) ->
  Separated = [{attribute,Pos,export,LI} || LI <- L],
  find_exports(R, Separated ++ Found, Attr);
find_exports([O|R], Found, Attr) ->
  find_exports(R, Found, [O | Attr]).

% Reformat atoms into all versions of that function if it exists, and reformat all
% funsigs into the format that erlang expects ({Name,Arity}).
expanded_exports(Exps, Avail) -> expanded_exports(Exps, Avail, []).
expanded_exports([],_Avail,Acc) -> ?rev(Acc);
expanded_exports([{attribute, _, export, {atom,Pos,What}}|R],Avail,Acc) ->
  expanded_exports(R,Avail,[{attribute,Pos,export,availables(Avail, What, [], Pos)}|Acc]);
expanded_exports([{attribute, _, export, {'fun',Pos,{function,Name,Arity}}}|R],Avail,Acc) ->
  expanded_exports(R,Avail,[{attribute,Pos,export,[{Name,Arity}]}|Acc]);
expanded_exports([{attribute, Pos, export, Err}|_],_,_) ->
  ?halt_error(Pos, "Unknown export type", Err, ?EXPORT_EXAMPLE).

% Look through list of available functions of one with this name and return
% all of them (with their arities, ready for the export statement)
availables([],Find,[], Pos) ->
  ?halt_error(Pos, "Exporting undefined function", Find, ?EXPORT_EXAMPLE);
availables([],_,Acc, _Pos)             -> ?rev(Acc);
availables([{Find,Arity}|R],Find,Acc,P)-> availables(R,Find,[{Find,Arity}|Acc],P);
availables([_|R],Find,Acc,P)           -> availables(R,Find,Acc,P).

fixup_module([],Acc) -> {none, none, ?rev(Acc)};
fixup_module([{attribute,_,module,Nm}=Mf|R],Acc) ->
  {Nm, Mf, ?rev(Acc) ++ R};
fixup_module([Attr|R],Acc) -> fixup_module(R,[Attr|Acc]).

