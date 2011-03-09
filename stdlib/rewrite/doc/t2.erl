-module(t2).
-compile(export_all).

a([L1,L2]=Original) ->
  case '--arwlsig1--'(L1,[]) of
    nomatch -> Original;
    {a,B_ARW,c} ->
      case '--arwlsig2--'(L2,[]) of
        nomatch -> Original;
        {d,M_M1,f} -> {right_side, B_ARW, M_M1}
      end
  end;
a(T)->T.

% TODO: foldl with first thing in Acc being the right-term, and the list being
% the lsigs. Build as follows:
% (case (call (atom ..) [(var ..)])
%   [(clause [(atom nomatch)] [] [(var Original)])
%    (clause [(tuple lsig-tuple)] [] [NEXT])])

'--arwlsig1--'(T,[]) when length(T) < 3 -> nomatch.
'--arwlsig2--'(T,[]) -> nomatch.
