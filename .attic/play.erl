-module(play).
-include("erlish.hrl").
-compile(export_all).

%testing(ModName) -> ?module(ModName).
%testing(MN, Params) -> ?module(MN, Params).

testing(Nm) ->
  ?forms([
    ?module,
    ?module(Nm),
    ?module(Nm, ["Name", "Date"]),
    ?export_list([{burn, 1}])
  ]).
