%% Copyright (C) 1996, Joe Armstrong
%% File    : tetris.erl
%% Author  : Joe Armstrong
%% Purpose : tetris
%% Usage   : tetris:start().

-module(tetris).

-vsn(1).
-author('joe@cslab.ericsson.se').

-export([start/1, show_high_score/0]).

-export([internal_1/1, ask3/3,
	 internal/2, internal_pinger/2, internal_speed_up/1,
	 show1/1, show2/2]).

-import(lists, [flatten/1, reverse/1, 
		nth/2, map/2, keymember/3, foreach/2, 
		seq/2, sort/1, filter/2]).

%% start the timer at 750 ms
%% then reduce by 6 ms every 10 seconds (i.e. 60 ms /min)
%% i.e. down to zero in 12.5 mins
%% reduce by 50 ms every time we go through a 25 boundary

-define(TimerInterval, 10000). %% every ten seconds
-define(DeltaTime,         6). %% reduce by 6 ms

start(0) -> true;
start(N) -> start(20, 750),start(N-1).

start(Size, Time) -> spawn(?MODULE, internal, [Size, Time]).

internal(Size, Time) ->
    W = 10*Size,
    H = 20*Size + 40,
    S = gs:start(),
    Win = gs:create(window, S, [{title, "Tetris"},
				{width,W}, {height,H}, {bg,pink},
				{keypress, true}]),
    put(quit, gs:create(button, quit,Win,[{label, {text,"quit"}}
					  ,{width,80},{height,25},
					  {x,10},{y,H-30}])),
    put(score, gs:create(label, Win, [{label, {text, "0"}},
				      {width, 40}, {height,25},
				      {x,100}, {y,H-30}])),
    put(theScore, 0),
    random_start(),
    make_squares(Win, Size),
    gs:config(Win, {map, true}),
    Piece = new_piece(Win),
    Pinger = spawn_link(?MODULE, internal_pinger, [Time, self()]),
    put(pinger, Pinger),
    spawn_link(?MODULE,  internal_speed_up, [Pinger]),
    loop(Win, Piece, []).

make_squares(Win, Size) ->
    foreach(fun(Y) ->
		   foreach(fun(X) ->
				  YY = (20-X)*Size,
				  XX = (Y-1) * Size,
				  B = gs:create(label,Win,
					    [{x,XX},{y,YY},{bg,pink},
					     {width,Size-1},{height, Size-1}]),
				  put({X,Y}, B)
			  end, seq(1,20))
	    end, seq(1,10)).

loop(W, Bit, State) ->
    receive
	{gs,quit,_,_,_}                -> true;
	{gs,W,keypress,[],[b|_]}     -> move(left,  W, Bit, State);
	{gs,W,keypress,[],[n|_]}     -> move(rot,   W, Bit, State);
	{gs,W,keypress,[],[m|_]}     -> move(right, W, Bit, State);
	drop_block                     -> move(down,  W, Bit, State);
	{gs,W,keypress,[],[space|_]} -> move(drop,  W, Bit, State);
	{gs,W,keypress,[],[d|_]}     -> move(down,  W, Bit, State);
	{gs,W,keypress,[],[question|_]} -> 
	    io:format("b -- left\nn -- rotate\nm -- right\nspace -- drop\n"),
	    loop(W, Bit, State);
	Any -> 
	    io:format("? for help\n"),
	    loop(W, Bit, State)
    end.

%%______________________________________________________________________
%% new_pos(Direction, Piece) -> Piece'

new_pos(down,  {P, Col, X, Y, R}) -> {P, Col, X-1, Y,   R};
new_pos(left,  {P, Col, X, Y, R}) -> {P, Col, X,   Y-1, R};
new_pos(right, {P, Col, X, Y, R}) -> {P, Col, X,   Y+1, R};
new_pos(rot,   {P, Col, X, Y, 4}) -> {P, Col, X,   Y,   1};
new_pos(rot,   {P, Col, X, Y, R}) -> {P, Col, X,   Y,   R+1};
new_pos(drop,  Piece)             -> new_pos(down, Piece).

%%______________________________________________________________________
%% move(Dirn, Win, Old, State) 
%%  tries to move the piece Old in the direction Dirn

move(Dirn, Win, Old, State) ->
    New  = new_pos(Dirn, Old),
    %% io:format("now ~p\n", [New]),
    case can_piece_move(New, State) of
	true ->
	    draw(Win, visable(Old), pink),
	    draw(Win, visable(New), color(New)),
	    case Dirn of 
		drop -> move(Dirn, Win, New, State);
		_    -> loop(Win, New, State)
	    end;
	false when Dirn == down ->
	    freeze(Win, Old, State);
	false when Dirn == drop ->
	    freeze(Win, Old, State);
	false ->
	    loop(Win, Old, State)
    end.

%%______________________________________________________________________
%% freeze(Win, Piece, State)
%%   gets called when Piece can no longer move

freeze(Win, Piece, State) ->
    {_,Color,_,_,_} = Piece,
    State1 = map(fun(X) -> {X,Color} end, covered(Piece)) ++ State,
    State2 = remove_full_rows(Win, State1),
    case State2 of
	[{{Row,_},_}|_] when Row > 20 ->
	    tetris_high_score_show("tetris.dat", get(theScore)),
	    game_over(Win);
	_ ->
	    loop(Win, new_piece(Win), State2)
    end.

%%______________________________________________________________________
%% remove_full_rows(Win, State) -> State'
%%   removes full rows after a piece has stopped falling
%%   updates the score

remove_full_rows(Win, State) -> 
    S1 = sort(State),
    remove_full_row(S1, Win, []).

remove_full_row([{{X,1},_},{{X,2},_},{{X,3},_},{{X,4},_},{{X,5},_},
		 {{X,6},_},{{X,7},_},{{X,8},_},{{X,9},_},{{X,10},_}|T], Win,
	       L) ->
    draw(Win, [{X,1},{X,2},{X,3},{X,4},{X,5},{X,6},{X,7},{X,8},
	       {X,9},{X,10}], pink),
    update_score(),
    L1 = shift_rows(T, Win, L),
    remove_full_rows(Win, L1);
remove_full_row([H|T], Win, L) ->
    remove_full_row(T, Win, [H|L]);
remove_full_row([], Win, L) ->
    L.

shift_rows([{{X,Y},Col}|T], Win, L) ->
    draw(Win, [{X,Y}], pink),
    draw(Win, [{X-1,Y}], Col),
    shift_rows(T, Win, [{{X-1,Y},Col}|L]);
shift_rows([], Win, L) ->
    L.

%%______________________________________________________________________
%% can_piece_move(Piece, State) -> Bool

can_piece_move(Piece, State) ->
    Covered = covered(Piece),
    aand(nnot(any(fun(Sq) -> keymember(Sq, 1, State) end, Covered)),
	 nnot(hit_side(Covered))).

%%______________________________________________________________________
%% hit_side([{X,Y}]) -> Bool

hit_side([{0,_}|_])  -> true;
hit_side([{_,0}|_])  -> true;
hit_side([{_,11}|_]) -> true;
hit_side([_|T])      -> hit_side(T);
hit_side([])         -> false.

%%______________________________________________________________________
%% start_cols(Shape, Rot) -> {MinCol, MaxCol}
%%%  given a  shape and a rotation compute the range of columns that
%%%  the the piece can start in 

start_cols(tee, 2) -> {1,9};
start_cols(tee, 4) -> {2,10};
start_cols(bar, 1) -> {1,10};
start_cols(bar, 2) -> {3,9};
start_cols(bar, 3) -> {2,10};
start_cols(bar, 4) -> {3, 9};
start_cols(box, _) -> {1,9};
start_cols(_, _)   -> {2, 9}.

%%----------------------------------------------------------------------
%% new_piece(Win) -> Piece
%%   generate a new piece and update the display

new_piece(Win) ->
    Shape = random_shape(),
    Rot   = random(1, 4),
    {Min, Max} = start_cols(Shape, Rot),
    Col = random(Min, Max),
    Piece =  {Shape, random_color(), 22, Col, Rot},
    draw_piece(Win, Piece),
    Piece.

%%______________________________________________________________________
%% draw_piece(Win, Piece).

draw_piece(Win, Piece) ->
    draw(Win, visable(Piece), color(Piece)).

draw(Win, Squares, Color) ->
    foreach(fun({I,J}) ->
		   gs:config(get({I,J}), [{bg, Color}])
	   end, Squares).

%%----------------------------------------------------------------------
%% visible(Piece) -> [{X,Y}] 
%%   a list of the squares which are covered and on the board

visable(B) ->  filter(fun(X) -> on_board(X) end, covered(B)).

%%______________________________________________________________________
%% covered(Piece) -> [{X,Y}]
%%   a list of the pieces which are covered

covered({tee,_,X,Y,1}) -> [{X,Y},{X-1,Y-1},{X-1,Y},{X-1,Y+1}];
covered({tee,_,X,Y,2}) -> [{X,Y},{X,Y+1},{X+1,Y+1},{X-1,Y+1}];
covered({tee,_,X,Y,3}) -> [{X,Y},{X+1,Y},{X+1,Y+1},{X+1,Y-1}];
covered({tee,_,X,Y,4}) -> [{X,Y},{X,Y-1},{X+1,Y-1},{X-1,Y-1}];
covered({l1,_,X,Y,1})  -> [{X+1,Y-1},{X+1,Y},{X+1,Y+1},{X,Y+1}];
covered({l1,_,X,Y,2})  -> [{X-1,Y-1},{X,Y-1},{X+1,Y-1},{X+1,Y}];
covered({l1,_,X,Y,3})  -> [{X-1,Y-1},{X-1,Y},{X-1,Y+1},{X,Y-1}];
covered({l1,_,X,Y,4})  -> [{X-1,Y+1},{X,Y+1},{X+1,Y+1},{X-1,Y}];
covered({l2,_,X,Y,1})  -> [{X+1,Y-1},{X+1,Y},{X+1,Y+1},{X,Y-1}];
covered({l2,_,X,Y,2})  -> [{X-1,Y-1},{X,Y-1},{X+1,Y-1},{X-1,Y}];
covered({l2,_,X,Y,3})  -> [{X-1,Y-1},{X-1,Y},{X-1,Y+1},{X,Y+1}];
covered({l2,_,X,Y,4})  -> [{X-1,Y+1},{X,Y+1},{X+1,Y+1},{X+1,Y}];
covered({bar,_,X,Y,1}) -> [{X,Y},{X-1,Y},{X+1,Y},{X+2,Y}];
covered({bar,_,X,Y,2}) -> [{X+1,Y},{X+1,Y+1},{X+1,Y-1},{X+1,Y-2}];
covered({bar,_,X,Y,3}) -> [{X+2,Y-1},{X-1,Y-1},{X,Y-1},{X+1,Y-1}];
covered({bar,_,X,Y,4}) -> [{X,Y-2},{X,Y-1},{X,Y},{X,Y+1}];
covered({r1,_,X,Y,1})  -> [{X,Y},{X+1,Y},{X,Y+1},{X-1,Y+1}];
covered({r1,_,X,Y,2})  -> [{X,Y},{X,Y-1},{X+1,Y},{X+1,Y+1}];
covered({r1,_,X,Y,3})  -> [{X,Y},{X+1,Y-1},{X,Y-1},{X-1,Y}];
covered({r1,_,X,Y,4})  -> [{X,Y},{X-1,Y-1},{X-1,Y},{X,Y+1}];
covered({r2,_,X,Y,1})  -> [{X,Y},{X+1,Y},{X-1,Y-1},{X,Y-1}];
covered({r2,_,X,Y,2})  -> [{X,Y},{X,Y-1},{X-1,Y+1},{X-1,Y}];
covered({r2,_,X,Y,3})  -> [{X,Y},{X-1,Y},{X+1,Y+1},{X,Y+1}];
covered({r2,_,X,Y,4})  -> [{X,Y},{X,Y+1},{X+1,Y},{X+1,Y-1}];
covered({box,_,X,Y,_})  -> [{X,Y},{X,Y+1},{X+1,Y},{X+1,Y+1}].

%%______________________________________________________________________
%% onboard({X,Y}) -> Bool
%%   test if a square is on the board

on_board({X,Y}) when 1 =< X, X =< 20, 1 =< Y, Y =< 10 -> true;
on_board(_)     -> false.

%%______________________________________________________________________
%% internal_pinger(T, Pid)
%%   every T ms sends a ping messaage to Pid
%%   the ping message will cause a block to drop

internal_pinger(T, Pid) ->
    receive
	{reduce, Delta} ->
	    internal_pinger(reduce_time(T - Delta), Pid)
    after T ->
	    Pid ! drop_block,
	    internal_pinger(T, Pid)
    end.

reduce_time(X) when X < 50 -> 50;
reduce_time(X) -> X.

%%______________________________________________________________________
%% internal_speed_up(Pid)
%%   every ?TimerInteral ms tell Pid to speed up by
%%   ?DeltaTime ms.

internal_speed_up(Pid) ->
    receive
    after ?TimerInterval ->
	    Pid ! {reduce, ?DeltaTime}
    end,
    internal_speed_up(Pid).

%%______________________________________________________________________
%% update_score()
%%   
%% every time bump the score by 10 and update the score
%% when we go through a 200 boundary (20 rows)
%% speed the clock up by 50 ms.

update_score() ->
    S = get(theScore) + 10,
    put(theScore, S),
    gs:config(get(score), [{label, {text, integer_to_list(S)}}]),
    case S rem 200 of
	0 -> get(pinger) ! {reduce, 50};
	_ -> true
    end.

%%______________________________________________________________________
%%

game_over(Win) ->
    gs:config(get(quit), [{label, {text, "Game over"}}]),
    wait_quit(Win).

wait_quit(Win) ->
    receive
	{gs, quit, _, _, _} ->
	    gs:destroy(Win),
	    exit(die);
	Any ->
	    wait_quit(Win)
    end.    

%%______________________________________________________________________
%% random things

random_start() ->
    {H,M,S} = erlang:now(),
    random:seed(S,H,M).

random_shape() -> random([r1,r2,l1,l2,tee,tee,bar,bar,box,box]).

random_color() -> random([red,blue,green,yellow,black,orange]). %% not pink !! 

random(L) -> nth(random:uniform(length(L)), L).

random(Min, Max) -> Min + random:uniform(Max-Min+1) - 1.

%%______________________________________________________________________
%% obvious !

any(F, [H|T]) ->
    case F(H) of
	true -> true;
	false -> any(F, T)
    end;
any(F, []) -> false.

aand(true, true) -> true;
aand(_,_)        -> false.

nnot(true) -> false;
nnot(false) -> true.

color({_,Color,_,_,_}) -> Color.


%% Usage   : tetris_high_score_show(File)
%%         :     pops up a high score table
%%         : tetris_high_score_show(File, Score)
%%         :     possible add Score to the high score table
%%         :     prompting if necessary
%%         : The high score table is scored in File

show_high_score() -> tetris_high_score_show("tetris.dat").

tetris_high_score_show(File) -> spawn_link(?MODULE, show1, [File]).

tetris_high_score_show(File, Score) -> 
    spawn_link(?MODULE, show2, [File, Score]).

show1(File) ->
    case file:read_file(File) of
	{error, _} ->
	    display_scores([]);
	{ok, Bin} ->
	    display_scores(binary_to_term(Bin))
    end.

show2(File, Score) ->
    case file:read_file(File) of
	{error, _} ->
	    add_score(File, Score, []);
	{ok, Bin} ->
	    T = binary_to_term(Bin),
	    case add_to_hs(Score, T) of
		true ->
		    add_score(File, Score, T);
		false ->
		    display_scores(T)
	    end
    end.

add_to_hs(_, T) when length(T) < 10 -> true;
add_to_hs(Score, T) ->
    case reverse(T) of
	[{Min,_,_}|_] when Score > Min -> 
	    true;
	_  -> 
	    false
    end.

add_score(File, Score, T) ->
    P = self(),
    ask("New high score!",
	"Name:", 
	fun(Str) -> P ! {name,Str} end),
    receive
	{name, Str} ->
	    T1 = trim(merge(Score, Str, T)),
	    file:write_file(File, term_to_binary(T1)),
	    display_scores(T1)
    end.

display_scores(T) ->
    Texts = map(fun({Score,Name,{Y,M,D}}) ->
		       [integer_to_list(Score),
			Name,
			flatten(io_lib:format("~4w-~2.2.0w-~2.2.0w", 
					      [Y,M,D]))]
	       end, T),
    display(Texts).

merge(Score, Name, []) -> [{Score,Name,date()}];
merge(Score, Name, [{S,N,D}|T]) when Score > S->
    [{Score,Name,date()},{S,N,D}|T];
merge(Score, Name, [H|T]) ->
    [H|merge(Score,Name,T)].

trim([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10|_]) ->
    [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10];
trim(X) ->
    X.

display(Text) ->
    spawn_link(?MODULE, internal_1, [Text]).

internal_1(Text) ->
    W = 365,
    H = 255,
    ColWidths = [60,200,100],
    Title = "Tetris hall of fame",
    S = gs:start(),
    Win = gs:create(window, S, [{width,W}, {height,H}, {title, Title}]),
    G   = gs:create(grid, Win, [{width, W-10},{height,H-30}, {x,2}, {y,2},
				{columnwidths, ColWidths},{hscroll,false},
				{vscroll, false},
				{rows, {1,10}}]),
    gs:create(button, ok, Win, [{label,{text,"dismiss"}},
				{width,100},{y,H-35}]),
    print(Text, 1, G),
    gs:config(Win, {map, true}),
    wait(Win).

print([], _, _) -> true;
print([[Score,Name,Date]|T], N, G) ->
    gs:create(gridline, G, [{row, N}, 
			    {text,{1,Score}},{text,{2,Name}},
			    {text,{3,Date}}]),
    print(T, N+1, G).


wait(Win) ->
    receive
	{gs,ok, _,_,_} ->
	    gs:destroy(Win);
	Any ->
	    io:format("got:~p\n", [Any]),
	    wait(Win)
    end.

ask(Title, Text, Fun) ->
    spawn_link(?MODULE, ask3, [Title, Text, Fun]).

ask3(Title, Text, Fun) ->
    S = gs:start(),
    Win = gs:create(window, S, [{width,260}, {height,75}, {title,Title}]),
    gs:create(button, ok,Win,[{label, {text,"ok"}},{width,80},{height,25},
			      {x,10},{y,40}]),
    gs:create(label, Win, [{label, {text, Text}}, {width, 40}, {height,25},
			   {x,10}, {y,10}]),
    E = gs:create(entry, Win, [{width, 170}, {height, 25},
			       {x,70}, {y,10}]),
    gs:config(Win, {map, true}),
    wait(Win, E, Fun).

wait(Win, E, Fun) ->
    receive
	{gs,ok, _,_,_} ->
	    Str = gs:read(E, text),
	    gs:destroy(Win),
	    Fun(Str);
	Any ->
	    io:format("got:~p\n", [Any]),
	    wait(Win, E, Fun)
    end.
