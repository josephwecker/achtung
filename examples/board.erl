-module(board).					% Program to roll a
-author('bjarne@cslab.ericsson.se').		% a boll on a board
-export([start/0,init/0,ball/1,beep_process/1,black_hole_process/1,
	 obstacle_process/1,save_browser/1,open_browser/1]).

-import(lists, [append/2,map/2,foreach/2,zf/2]).

-define(W, 600).				% Width of board
-define(Halfw, 300).

-define(H, 550).				% Height of board
-define(Halfh, 225).

-define(Winpos, 0).				% Position of buttons
-define(Losspos, ?W/6).				% and text at the
-define(Editpos, 2*?W/6).			% bottom of the board
-define(Newpos, 3*?W/6).
-define(Origpos, 4*?W/6).
-define(Stoppos, 5*?W/6).

-define(Ball, 40).				% Size of ball
-define(Halfball, 20.0).			% Ball position = centre

-define(Xmax, 580.0).				% = W - Halfball
-define(Ymax, 530.0).				% = H - Halfball

-define(Ymin, 20.0).				% = Halfball
-define(Xmin, 20.0).				% = Halfball

-define(Xinit, 560.0).				% = Xmax - Ball
-define(Yinit, 40.0).				% = Ball
-define(Initcorner, {gs,1,?Xinit-?Halfball,0,?W,?Yinit+?Halfball}).

-define(Xwin, 40.0).				% = Ball
-define(Ywin, 490.0).				% = Ymax - Ball
-define(Wincorner, {gs,1,0,?Ywin,?Xwin,?Ymax}).

-define(Obs, [{250, 100, 550, 150},		% Obstacles in original game
	      {100, 100, 150, 300},		% {a,b,c,d} where
	      {300, 280, 400, 380},		% (a,b) = upper left corner
	      {  0, 420, 230, 470}]).		% (c,d) = lower right corner

-define(Holes, [{  0,   0, 100, 100},	 	% Black holes in 
	        {500, 250, 600, 350},		% original game
	        {350, 470, 450, 550}]).

-define(Time, 10).				% Ball position update interval
-define(Step, 20.0).
-define(Timezoom, 100).				% Collapse interval in hazard
-define(Timeflash, 200).			% Flash interval when winning
-define(Timebeep,300).				% Shortest beep interval

-record(buttons, {stop,game,open,save,save_as}). % Edit buttons
-record(butts, {stop,new,edit,orig}).		% Game buttons
-record(data, {wing,lossg,ball,win,loss,x,y,dx,dy}). % Ball parameters

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Main program
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> 
    register(board,spawn(board,init,[])).

init() ->					% Start up function 
    GS = gs:start(),				% Initialise graphics
    Win=gs:create(window,GS,[{width,?W},{height,?H+25},
			     {map,true},{keypress,false},{keyrelease,false}]),
    Canvas=gs:create(canvas,Win,[{width,?W},{height,?H},{bg,yellow}]),
    gs:create(text,Canvas,[{coords,[{round(?Xmax)-25,25}]},{text,"Start"}]),
    gs:create(text,Canvas,[{coords,[{25,round(?Ymax)-25}]},{text,"Finish"}]),
    gs:config(Canvas,[{motion,true}]),
    
    reg(obstacles,spawn(board,obstacle_process,[{obstacles,[]}])), % Start
    reg(black_holes,spawn(board,black_hole_process,[{holes,[]}])), % processes
    reg(beep,spawn(board,beep_process,[Win])),
    
    {Wins,Loss} = init3(Canvas,Win,0,0), 	% Play games and draw boards
    
    io:format("You win ~w and loose ~w ~nEnd of game~n",[Wins,Loss]),
    black_holes ! game_over,			% Terminate processes
    obstacles ! game_over,
    beep ! game_over,
    exit(normal).

reg(Name,Pro) ->
    case whereis(Name) of
	undefined -> register(Name,Pro);
	_ -> Name ! game_over,
	     unregister(Name),
	     register(Name,Pro)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Main program for creating a new board
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init1(Canvas,Win,Wins,Loss) ->			% Make a new board
						% Canvas,Win = gs objects
						% Wins, Loss = play results
						% Returns {Wins,Loss}
    [Stop,Game,Open,Save,Save_as] =		% Create gs buttons
	create_buttons(Win,[{?Stoppos,"Stop"},{?Origpos,"Game"},
			    {?Newpos,"Open"},{?Editpos,"Save"},
			    {?Losspos,"Save as"}]),
    gs:config(Canvas,[{buttonpress,true},{buttonrelease,true}]),
    gs:config(Win,[{keypress,true}]),
    Recs = fetch_recs(),    			% Fetch rectangles
    
    Draw = mk_board(Win,Canvas,#buttons{stop=Stop,game=Game,open=Open,
					save=Save,save_as=Save_as},Recs),
						% Run drawing and editing    
    
    gs:config(Win,[{keypress,false}]),
    gs:config(Canvas,[{buttonpress,false},{buttonrelease,false}]),
    foreach(fun(X) -> gs:destroy(X) end, [Stop,Game,Open,Save,Save_as]),
    
    case Draw of				% Exit conditions
	exit -> {Wins,Loss};			% Finish program
	{game,Rectangles} ->			% Want to play a game
	    store_recs(Rectangles),		% Store rectangles
	    init2(Canvas,Win,Wins,Loss);
	_ -> {Wins,Loss}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fetch_recs() ->					% Make a list of rectangles
    map(fun({Rec,X,A,B,C,D}) ->
		gs:config(Rec,[{enter,true},{leave,true}]),
		case X of
		    1 -> {Rec,1,round(A+?Halfball),round(B+?Halfball),
			  round(C-?Halfball),round(D-?Halfball)};
		    _ -> {Rec,3,round(A),round(B),round(C),round(D)}
		end 
	end, 
	append(black_holes(),obstacles())).	% from holes and obstacles

store_recs(Rectangles) ->			% Store rectangles 
    map(fun({Rec,_,_,_,_,_}) -> 
		gs:config(Rec,[{enter,false},{leave,false}])
	end,
	Rectangles),
    obstacles !					% as obstacles
	{put_obs,
	 {obstacles,
	  zf(fun({Rec,1,A,B,C,D}) ->
		     {true,{Rec,1,float(A)-?Halfball,float(B)-?Halfball,
			    float(C)+?Halfball,float(D)+?Halfball}};
		(_) -> false
	     end,
	     Rectangles)}},
    black_holes !				% and as black holes
	{put_holes,
	 {holes,
	  zf(fun({Rec,3,A,B,C,D}) ->
		     {true,{Rec,3,float(A),float(B),float(C),float(D)}};
		(_) -> false
	     end, 
	     Rectangles)}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Rectangle drawing loop
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mk_board(Win,Canvas,Buttons,R) -> 		% Board making loop;   Win,
						% Canvas,Buttons = gs objects
						% R = list of rectangles
    receive
	{gs,_,buttonpress,_,[X,A,B|_]} -> 	% Mouse button for
	    Rec = gs:create(rectangle,Canvas,	% new rectangle
			    [{coords,[{A,B},{A,B}]},{fill,colour(X)},
			     {enter,true},{leave,true}]),
	    mk_board(Win,Canvas,Buttons,R,Rec,mouse(X),A,B);
	{gs,Rec,enter,_,_} ->			% Pointing at rectangle
	    mk_board(Win,Canvas,Buttons,R,Rec);
	{gs,Button,click,_,_} ->		% Button pressed
	    button(Button,Win,Canvas,Buttons,R);
	{gs,_,destroy,_,_} ->			% Kill window
	    exit;
	{gs,_,_,_,_} ->				% Ignored
	    mk_board(Win,Canvas,Buttons,R);
	Msg ->					% Other message
	    io:format("~w ~n",[Msg]),
	    mk_board(Win,Canvas,Buttons,R)
    end.

mk_board(Win,Canvas,Buttons,R,Rec,X,A,B) -> 	% New rectangle being made
						% (A,B) = starting corner
						% X = 1 for blue, 3 for black
    receive					
	{gs,_,motion,_,[C,D|_]} ->		% Cursor movement
	    gs:config(Rec,{coords,[{A,B},{C,D}]}),
	    mk_board(Win,Canvas,Buttons,R,Rec,X,A,B);
	{gs,_,buttonrelease,[],[_,C,D|_]} ->	% Rectangle finished
	    Overlap = overlap({min(A,C),min(B,D),max(A,C),max(B,D)},R),
	    if abs(A-C) > 5, abs(B-D) > 5, Overlap == false ->
		    gs:config(Rec,{coords,[{A,B},{C,D}]}),
		    mk_board(Win,Canvas,Buttons,
			     [{Rec,X,min(A,C),min(B,D),max(A,C),max(B,D)}|R]);
	       true ->
		    gs:destroy(Rec),
		    mk_board(Win,Canvas,Buttons,R)
	    end;
	{gs,Button,click,_,_} ->		% Button pressed
	    gs:destroy(Rec),
	    button(Button,Win,Canvas,Buttons,R);
	{gs,_,destroy,_,_} ->			% Kill window
	    exit;
	{gs,_,_,_,_} ->				% Ignored
	    mk_board(Win,Canvas,Buttons,R,Rec,X,A,B);
	Msg ->					% Other message
	    io:format("~w ~n",[Msg]),
	    mk_board(Win,Canvas,Buttons,R,Rec,X,A,B)
    end.

mk_board(Win,Canvas,Buttons,R,Rec)->		% Rectangle Rec pointed at 
    receive					
	{gs,_,buttonpress,_,[Xn,An,Bn|_]} -> 	% Mouse button 
	    case find(Rec,R) of 		% catching rectangle
		{Rec,X,A,B,C,D} ->
		    if An - A + Bn - B < C - An + D - Bn ->
			    gs:config(Rec,[{coords,[{C,D},{An,Bn}]},
					   {fill,colour(Xn)}]),
			    mk_board(Win,Canvas,Buttons,
				     remove(Rec,R),Rec,mouse(Xn),C,D);
		       true ->
			    gs:config(Rec,[{coords,[{A,B},{An,Bn}]},
					   {fill,colour(Xn)}]),
			    mk_board(Win,Canvas,Buttons,
				     remove(Rec,R),Rec,mouse(Xn),A,B)
		    end;
		fail -> mk_board(Win,Canvas,Buttons,R);
		_ -> mk_board(Win,Canvas,Buttons,R)
	    end;
	{gs,_,keypress,_,['Delete'|_]} ->	% Delete rectangle
	    case find(Rec,R) of
		{Rec,_,_,_,_,_} -> gs:destroy(Rec),
				   mk_board(Win,Canvas,Buttons,remove(Rec,R));
		_ -> mk_board(Win,Canvas,Buttons,R)
	    end;
	{gs,Button,click,_,_} ->		% Button pressed
	    button(Button,Win,Canvas,Buttons,R);
	{gs,Recn,enter,_,_} ->			% Pointing at rectangle
	    mk_board(Win,Canvas,Buttons,R,Recn);
	{gs,_,leave,_,_} ->			% Continue
	    mk_board(Win,Canvas,Buttons,R);
	{gs,_,destroy,_,_} ->			% Kill window
	    exit;
	{gs,_,_,_,_} ->				% Ignored
	    mk_board(Win,Canvas,Buttons,R,Rec);
	Msg ->					% Other message
	    io:format("~w ~n",[Msg]),
	    mk_board(Win,Canvas,Buttons,R,Rec)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

button(Button,Win,Canvas,Buttons,R) ->		% Button pressed
    if Button == Buttons#buttons.stop -> exit;	% Stop button 
       Button == Buttons#buttons.game -> {game,R}; % Game button
       Button == Buttons#buttons.open -> 	% Open file button
	    case open(Canvas,R) of
		{File,Rnew} -> gs:config(Win,{title,"Roll board - " ++ File}),
			       mk_board(Win,Canvas,Buttons,Rnew);
		_ -> mk_board(Win,Canvas,Buttons,R)
	    end;
       Button == Buttons#buttons.save-> 	% Save file button
	    save(current_file(Win),R),
	    mk_board(Win,Canvas,Buttons,R);
       Button == Buttons#buttons.save_as -> 	% Save as button
	    case save_as(R) of
		{file,File} -> gs:config(Win,{title,"Roll board - " ++ File});
		_ -> true
	    end,
	    mk_board(Win,Canvas,Buttons,R);
       true -> io:format("{gs,~w,click} ~n",[Button]),
	       mk_board(Win,Canvas,Buttons,R)
    end.

mouse(1) -> 1;					% Left mouse button
mouse(_) -> 3.					% Other mouse button

colour(1) -> blue;				% Obstacle
colour(_) -> black.				% Black hole

current_file(Win) ->				% Current file name
    Title = gs:read(Win,title),			% is kept in the title
    File = file_name(Title,"Roll board - "),	% of the board window
    if File == "orginal board" -> "board_save";
       File == "new board" -> "board_save";
       true -> File
    end.

file_name(Title,[]) -> Title;
file_name([_|T],[_|R]) -> file_name(T,R).

overlap(Coords,R) ->				% Check for overlap with
    overlap1(Coords,[?Initcorner,?Wincorner|R]). % start and win positions

overlap1(_,[]) -> false;			% and existing rectangles
overlap1({A,B,C,D},[{_,_,Ax,Bx,Cx,Dx}|T]) ->	
    if Ax < C, A < Cx, Bx < D, B < Dx -> true;
       true -> overlap1({A,B,C,D},T)
    end.

max(X,Y) when X > Y -> X;
max(X,Y) -> Y.

min(X,Y) when X < Y -> X;
min(X,Y) -> Y.

find(Rec,[]) -> fail;				% Find Rec in list of 
find(Rec,[{Rec,X,A,B,C,D}|_]) -> {Rec,X,A,B,C,D}; % rectangles 
find(Rec,[H|T]) -> find(Rec,T).

remove(Rec,[]) -> [];				% Remove Rec from list
remove(Rec,[{Rec,_,_,_,_,_}|T]) -> T;		% of rectangles
remove(Rec,[H|T]) -> [H|remove(Rec,T)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Functions to deal with files
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open(Canvas,Recs) ->				% Read file; Canvas = gs object
						% Recs = current rectangles
						% Return new file name and 
						% rectangles read from file
    B = spawn(board,open_browser,[self()]),	% Create list box
    receive
	{open_browser,cancel} -> cancel;		% Cancel
	{open_browser,text,File} ->		% File name given
	    case file:consult(File ++ ".board") of
		{ok,Recnew} ->		
		    foreach(fun({Rec,_,_,_,_,_}) -> gs:destroy(Rec) end, Recs),
		    {File,load_recs(Canvas,Recnew)}; % Load rectangles
		_ -> error
	    end			
    end.

load_recs(Canvas,[]) -> [];			% Check data from file
load_recs(Canvas,[{X,A,B,C,D}|T]) ->		% and create rectangles
    case integers([X,A,B,C,D]) of		
	yes -> if 0 =< A, A < C, C =< ?W, 0 =< B, B < D, D =< ?H -> 
		       [{gs:create(
			   rectangle,Canvas,[{coords,[{A,B},{C,D}]},
					     {fill,colour(X)},
					     {enter,true},{leave,true}]),
			 mouse(X),A,B,C,D} | load_recs(Canvas,T)];
		  true -> load_recs(Canvas,T)
	       end;
	_ -> load_recs(Canvas,T)
    end;
load_recs(Canvas,[_|T]) -> load_recs(Canvas,T).

integers([]) -> yes;				% Check for integers 
integers([H|T]) when integer(H) -> integers(T);
integers(_) -> no.

open_browser(Board) ->				% Create listbox of
    S = gs:start(),				% file names
    Win = gs:window(S,[{width,250},{height,270},{title,"Board files"}]),
    Lab = gs:label(Win,[{label,{text,"Which file to open ?"}},{width,250}]),
    Browse = gs:listbox(Win,[{x,5},{y,65},{width,160},{height,195},
			     {vscroll,right},{click,true},{doubleclick,true}]),
    Cancel = gs:button(Win,[{label,{text,"Cancel"}},
			    {x,175},{y,225},{width,65}]),
    gs:config(Browse,[{items,lists:sort(file_list())}]),
    gs:config(Win,{map,true}),
    open_browser(Board,Cancel,Browse).

open_browser(Board,Cancel,Browse) ->		% Await player response
    receive
	{gs,Cancel,click,_,_} ->		% Cancel
	    Board ! {open_browser,cancel};      
	{gs,Browse,click,_,[_,File|_]} ->	% Select name in listbox
	    Board ! {open_browser,text,File};
	{gs,_,destroy,_,_} ->			% Listbox killed
	    Board ! {open_browser,cancel};
	Msg ->					% Other message
	    io:format("~w ~n",[Msg]),
	    open_browser(Board,Cancel,Browse)
    end,
    exit(normal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save(File,Recs) ->				% Save in current file
						% Input current file name and 
						% list of rectangles
    Fdesc = file:open(File ++ ".board",write),
    case Fdesc of
	{ok,F} ->
	    save_recs(F,Recs),
	    file:close(F);
	_ -> true
    end.

save_recs(F,Recs) ->				% Write rectangles to file
    foreach(fun({_,X,A,B,C,D}) -> 
		    io:format(F,"~w.~n",[{mouse(X),max(A,0),max(B,0),
					  min(C,?W),min(D,?H)}]) 
	    end, 
	    Recs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save_as(Recs) ->				% Save in appointed file
						% Recs = current rectangles
						% Return file name
    spawn(board,save_browser,[self()]),	
    receive				
	{save_browser,cancel} -> cancel;
	{save_browser,text,File} ->
	    case check_name(1,File) of
		ok -> 
		    case file:open(File ++ ".board",write) of
			{ok,F} ->
			    save_recs(F,Recs),
			    file:close(F),
                            {file,File};
			_ -> error
		    end;
		_ -> error
	    end
    end.

check_name(_,[]) -> ok;				% Check file name from player
check_name(X,[H|T]) ->
    if [H] >= "0", [H] =< "9" -> check_name(2,T); % Numerals
       [H] >= "A", [H] =< "Z" -> check_name(2,T); % Big letters
       [H] >= "a", [H] =< "z" -> check_name(2,T); % Small letters
       [H] == "_", X == 2 -> check_name(2,T);	% Underscore
       true -> error
    end.

save_browser(Board) ->				% Create listbox
    S = gs:start(),				% gs manual figure 5.19
    Win = gs:window(S,[{width,250},{height,270},{title,"Board files"}]),
    Lab = gs:label(Win,[{label,{text,"Which save file ?"}},{width,250}]),
    Entry = gs:entry(Win,[{y,35},{width,240},{x,5},
			  {keypress,true},{setfocus,true}]),
    Browse = gs:listbox(Win,[{x,5},{y,65},{width,160},{height,195},
			     {vscroll,right},{click,true},{doubleclick,true}]),
    Ok = gs:button(Win,[{label,{text,"OK"}},{width,175},
			{x,175},{y,175},{width,65}]),
    Cancel = gs:button(Win,[{label,{text,"Cancel"}},
			    {x,175},{y,225},{width,65}]),
    gs:config(Browse,[{items,lists:sort(file_list())}]),
    gs:config(Win,{map,true}),
    save_browser(Board,Ok,Cancel,Entry,Browse).

save_browser(Board,Ok,Cancel,Entry,Browse) ->	% Await player response
    receive
	{gs,Cancel,click,_,_} -> 
	    Board ! {save_browser,cancel};	% Cancel
	{gs,Ok,click,_,_} -> 
	    File = gs:read(Entry,text),
	    Board ! {save_browser,text,File};	% Written file name
	{gs,Entry,keypress,_,['Return'|_]} ->
	    File = gs:read(Entry,text),
	    Board ! {save_browser,text,File};	% Written file name
	{gs,Entry,keypress,_,_} ->
	    save_browser(Board,Ok,Cancel,Entry,Browse);
	{gs,Browse,click,_,[_,File|_]} ->
	    Board ! {save_browser,text,File};	% Clicked file name
	{gs,_,destroy,_,_} ->
	    Board ! {save_browser,cancel};
	Msg ->					% Other message
	    io:format("~w ~n",[Msg]),
	    save_browser(Board,Ok,Cancel,Entry,Browse)
    end,
    exit(normal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_list() ->					% Fetch files in
    case file:list_dir(".") of			% current directory
	{ok,T} -> file_list(T);
	_ -> []
    end.

file_list([]) -> [];				% Check file extension
file_list([H|T]) ->				% to be .board
    Hlen = lists:flatlength(H),
    if [hd(H)] == "." -> file_list(T);
       Hlen < 7 -> file_list(T);
       true ->
	    [D,R,A,O,B,P|F] = lists:reverse(H),
	    if [D,R,A,O,B,P] == "draob." ->
		    [lists:reverse(F)|file_list(T)];
	       true -> file_list(T)
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Main program for playing a game
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init3(Canvas,Win,Wins,Loss) ->			% Play on original board
    store_original_recs(Canvas),
    gs:config(Win,{title,"Roll board - orginal board"}),
    init2(Canvas,Win,Wins,Loss).

init2(Canvas,Win,Wins,Loss) ->			% Play a game
						% Canvas, Win = gs objects;
						% Wins, Loss = play results
						% Returns new {Wins,Loss}
						% init2 expects the board to 
						% be held by the black_holes
						% and obstacles processes
    [Wing,Losses] = 
	create_labels(Win,[{?Winpos,"You win " ++ integer_to_list(Wins)},
			   {?Losspos,"You loose " ++ integer_to_list(Loss)}]),
    [Stop,New,Edit,Orig] = 
	create_buttons(Win,[{?Stoppos,"Stop"},{?Newpos,"New board"},
			    {?Editpos,"Edit board"},{?Origpos,"Orig board"}]),
    Line = gs:create(line,Canvas),		% Create gs line
    Ball = gs:create(oval,Canvas,[{fill,red}]),	% Create gs ball
    
    register(ball,spawn(board,ball,		% Start ball process
			[#data{wing = Wing,	% gs object text
			       lossg = Losses,	% gs object text
			       ball = Ball,	% gs object ball
			       win = Wins,	% No of wins
			       loss = Loss,	% No of losses
			       x = ?Xinit,	% Present position =
			       y = ?Yinit,	% middle of the ball
			       dx = 0.0,	% Increment of position
			       dy = 0.0}])),
    
    Run = board(Line,#butts{stop=Stop,new=New,edit=Edit,orig=Orig},0.0,0.0), 
						% Play !
    
    foreach(fun(X) -> gs:destroy(X) end, 
	    [Wing,Losses,Stop,New,Edit,Orig,Line,Ball]),
    
    case Run of					% Exit conditions
	{exit,Winsn,Lossn} -> {Winsn,Lossn};	% Game over
	{new,Winsn,Lossn} -> 			% Make a new board
	    clear_board(),
	    gs:config(Win,{title,"Roll board - new board"}),
	    init1(Canvas,Win,Winsn,Lossn);
	{edit,Winsn,Lossn} -> 			% Edit present board
	    case gs:read(Win,title) of
		"Roll board - orginal board" ->
		    gs:config(Win,{title,"Roll board - new board"}),
		    init1(Canvas,Win,Winsn,Lossn);	
		_ -> init1(Canvas,Win,Winsn,Lossn)
	    end;
	{original,Winsn,Lossn} -> 		% Run on original board
	    clear_board(),
	    init3(Canvas,Win,Winsn,Lossn);
	_ -> {Wins,Loss}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_original_recs(Canvas) ->			% Store original
    black_holes ! 				% black holes
	{put_holes,
	 {holes,
	  map(fun({A,B,C,D}) -> 
		      {gs:create(rectangle,Canvas,[{coords,[{A,B},{C,D}]},
						   {fill,black}]),
		       3,round(A),round(B),round(C),round(D)}
	      end,?Holes)}},
    obstacles ! 				% and obstacles
	{put_obs,
	 {obstacles,
	  map(fun({A,B,C,D}) -> 
		      {gs:create(rectangle,Canvas,[{coords,[{A,B},{C,D}]},
						   {fill,blue}]),
		       1,round(A-?Halfball),round(B-?Halfball),
		       round(C+?Halfball),round(D+?Halfball)}
	      end,?Obs)}}.

clear_board() -> 				% Clear holes and obstacles
    foreach(fun({Rec,_,_,_,_,_}) -> gs:destroy(Rec) end,
	    append(obstacles(),black_holes())),
    black_holes ! {put_holes,{holes,[]}},
    obstacles ! {put_obs,{obstacles,[]}}.

create_buttons(Win,[]) -> [];			% Create buttons at the lower
create_buttons(Win,[{Xpos,Text}|T]) -> 		% edge of the board
    [gs:create(
       button,Win,
       [{x,Xpos},{y,?H},{width,100},{height,25},{label,{text,Text}}]) |
     create_buttons(Win,T)].

create_labels(Win,[]) -> []; 			% Create labels at the lower
create_labels(Win,[{Xpos,Text}|T]) -> 		% edge of the board
    [gs:create(
       label,Win,
       [{x,Xpos},{y,?H},{width,100},{height,25},{label,{text,Text}}]) |
     create_labels(Win,T)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Board process tracking the mouse
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

board(Line,Buttons,Tiltx,Tilty) ->		% Game board loop
    receive
	{gs,_,motion,[],[Mousex,Mousey,_,_]} ->	% Extract tilt from mouse
	    gs:config(Line,{coords,[{Mousex,Mousey},{?Halfw,?Halfh}]}),
	    board(Line,Buttons,Mousex - ?Halfw,Mousey - ?Halfh);
	what ->					% Send current tilt
	    ball ! {float(Tiltx),float(Tilty)},	% to the ball process
	    board(Line,Buttons,Tiltx,Tilty);
	flush ->				% Flush mouse moves
	    flush_mouse(),
	    board(Line,Buttons,0,0);
	{gs,Button,click,_,_} ->		% Button pressed
	    click(Button,Line,Buttons,Tiltx,Tilty);
	{gs,_,destroy,_,_} ->			% Kill window
	    return(exit);
	Msg ->					% Other message
	    io:format("~w ~n",[Msg]),
	    board(Line,Buttons,Tiltx,Tilty)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

click(Button,Line,Buttons,Tiltx,Tilty) ->	% Button pressed
    if Button == Buttons#butts.stop ->		% Stop button
	    return(exit);
       Button == Buttons#butts.new ->		% Button to make a
	    return(new);				%   new board
       Button == Buttons#butts.edit ->		% Button to edit board
	    return(edit);
       Button == Buttons#butts.orig ->		% Button for new game 
	    return(original);			%  on original board
       true -> io:format("{gs,~w,click} ~n",[Button]),
	       board(Line,Buttons,Tiltx,Tilty)
    end.

return(Answer) ->				% Button pressed
    ball ! game_over,				% Stop ball process
    receive {Wins,Loss} -> {Answer,Wins,Loss} 	% Get current results
    end.

flush_mouse() ->				% Flush mouse moves
    receive					% before next game
	{gs,_,motion,_,_} -> flush_mouse()
    after 0 -> true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Ball process
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ball(Data) ->				
    gs:config(Data#data.ball,{coords,[{round(Data#data.x - ?Halfball),
				       round(Data#data.y - ?Halfball)},
				      {round(Data#data.x + ?Halfball),
				       round(Data#data.y + ?Halfball)}]}),
    case test(Data#data.x,Data#data.y,black_holes()) of
	cont ->					% Continue the game
	    receive
		game_over -> 
		    board ! {Data#data.win,Data#data.loss},
		    exit(normal)
	    after ?Time -> true
	    end,
	    board ! what,
	    receive
		{Tiltx,Tilty} ->		% Fetch current tilt
		    {Xn,Yn,Dxn,Dyn} =		% Compute new coordinates
			bounce_test_left_right(
			  Data#data.x+new_delta(Tiltx,Data#data.dx),
			  Data#data.y+new_delta(Tilty,Data#data.dy),
			  Data#data.x,Data#data.y),
		    ball(Data#data{x = Xn, y = Yn, dx = Dxn, dy = Dyn});
		game_over -> 
		    board ! {Data#data.win,Data#data.loss},
		    exit(normal)
	    end;
	win ->					% Reached winning position
	    flash(Data#data.ball,
		  [yellow,green,yellow,green,yellow,green,yellow,red]),
	    Wn = Data#data.win + 1,
	    gs:config(Data#data.wing,
		      {label,{text,"You win " ++ integer_to_list(Wn)}}),
	    board ! flush,
	    ball(Data#data{x = ?Xinit, y = ?Yinit, win = Wn});	
	{A,B,C,D} ->				% Fallen into a black hole
	    zoom(Data#data.ball,Data#data.x,Data#data.y,A,B,C,D),
	    Ln = Data#data.loss + 1,
	    gs:config(Data#data.lossg,
		      {label,{text,"You loose " ++ integer_to_list(Ln)}}),
	    receive
		game_over -> 
		    board ! {Data#data.win,Data#data.loss},
		    exit(normal)
	    after 0 -> true
	    end,
	    board ! flush,
	    ball(Data#data{x = ?Xinit, y = ?Yinit, loss = Ln})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(X,Y,[]) ->					% Test ball position
    if X < ?Xwin, Y > ?Ywin -> win;		% Win !
       true -> cont				% Continue game
    end;
test(X,Y,[{_,_,A,B,C,D}|T]) ->			% Test for hazard
    if A < X, X < C, B < Y, Y < D -> {A,B,C,D};	% Fallen down !
       true -> test(X,Y,T)
    end.

new_delta(Tilt,Delta) ->			% Compute new increment
    if Tilt * Delta < 0.0 -> Delta + Tilt / 3000.0; 
       Delta > ?Xmax -> ?Xmax;			% delta{x,y} based on 
       Delta < -?Xmax -> -?Xmax;		% previous delta and 
       true -> Delta + Tilt / 10000.0		% current tilt
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Functions to check for bounces 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bounce_test_left_right(Xn,Yn,X,Y) ->		% Test for horisontal bounces
						% (X,Y) = old position 
						% (Xn,Yn) computed new pos
						%  according to tilt
						% Returns {Xr,Yr,Dx,Dy} where
						%  (Xr,Yr) is the new position
						%  after possible bounces
						% and (Dx,Dy) the new delta
    if Xn > X -> test_from_left(obstacles(),Xn,Yn,X,Y);
       Xn < X -> test_from_right(obstacles(),Xn,Yn,X,Y);
       true -> bounce_test_up_down(Xn,Yn,X,Y)
    end.

test_from_left([],Xn,Yn,X,Y) -> bounce_test_up_down(Xn,Yn,X,Y);
test_from_left([{_,_,A,B,C,D}|T],Xn,Yn,X,Y) ->
    if X =< A, A < Xn -> Bn = ((A - X) * (Yn - Y)) / (Xn - X) + Y,
			 if B =< Bn, Bn =< D -> beep ! beep, 
						{A,Bn,X-Xn,Yn-Y};
			    true -> test_from_left(T,Xn,Yn,X,Y)
			 end;
       true -> test_from_left(T,Xn,Yn,X,Y)
    end.

test_from_right([],Xn,Yn,X,Y) -> bounce_test_up_down(Xn,Yn,X,Y);
test_from_right([{_,_,A,B,C,D}|T],Xn,Yn,X,Y) ->
    if X >= C, C > Xn-> Bn = ((C - X) * (Yn - Y)) / (Xn - X) + Y,
			if B =< Bn, Bn =< D -> beep ! beep, 
					       {C,Bn,X-Xn,Yn-Y};
			   true -> test_from_right(T,Xn,Yn,X,Y)
			end;
       true -> test_from_right(T,Xn,Yn,X,Y)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bounce_test_up_down(Xn,Yn,X,Y) ->		% Test for vertical bounces
    if Yn > Y -> test_from_above(obstacles(),Xn,Yn,X,Y);
       Yn < Y -> test_from_below(obstacles(),Xn,Yn,X,Y);
       true -> bounce_test_edges(Xn,Yn,X,Y)
    end.

test_from_above([],Xn,Yn,X,Y) -> bounce_test_edges(Xn,Yn,X,Y);
test_from_above([{_,_,A,B,C,D}|T],Xn,Yn,X,Y) ->
    if Y =< B, B < Yn -> An = ((B - Y) * (Xn - X)) / (Yn - Y) + X,
			 if A =< An, An =< C -> beep ! beep, 
						{An,B,Xn-X,Y-Yn};
			    true -> test_from_above(T,Xn,Yn,X,Y)
			 end;
       true -> test_from_above(T,Xn,Yn,X,Y)
    end.

test_from_below([],Xn,Yn,X,Y) -> bounce_test_edges(Xn,Yn,X,Y);
test_from_below([{_,_,A,B,C,D}|T],Xn,Yn,X,Y) ->
    if Y >= D, D > Yn -> An = ((D - Y) * (Xn - X)) / (Yn - Y) + X,
			 if A =< An, An =< C -> beep ! beep, 
						{An,D,Xn-X,Y-Yn};
			    true -> test_from_below(T,Xn,Yn,X,Y)
			 end;
       true -> test_from_below(T,Xn,Yn,X,Y)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bounce_test_edges(Xn,Yn,X,Y) ->			% Test for bounce into
    {if Xn > ?Xmax -> beep ! beep, ?Xmax;	% the edge of the board
        Xn < ?Xmin -> beep ! beep, ?Xmin;
        true -> Xn 
     end,
     if Yn > ?Ymax -> beep ! beep, ?Ymax;
        Yn < ?Ymin -> beep ! beep, ?Ymin;
        true -> Yn
     end,
     if Xn =< ?Xmax, Xn >= ?Xmin -> Xn-X;
        true -> X-Xn
     end,
     if Yn =< ?Ymax, Yn >= ?Ymin -> Yn-Y;
        true -> Y-Yn
     end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Functions for ball flashing when winning or falling into a hole
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flash(Ball,[C|T]) ->				% Flash the ball
    gs:config(Ball,{fill,C}),			% yellow and green
    if T == [] -> beep ! beep;			% when winning
       true -> receive after ?Timeflash -> true end,
	       flash(Ball,T)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zoom(Ball,X,Y,A,B,C,D) ->			% Zoom down into 
    Xinc = (X - (C + A) / 2.0) / ?Step,		% a black hole
    Yinc = (Y - (B + D) / 2.0) / ?Step,
    Rinc = ?Halfball / ?Step,
    zoom(?Step,Ball,X,Xinc,Y,Yinc,?Halfball,Rinc).

zoom(Step,Ball,_,_,_,_,_,_) when Step =< 0.0 -> beep ! beep;
zoom(Step,Ball,X,Xinc,Y,Yinc,R,Rinc) ->
    receive after ?Timezoom -> true end,
    gs:config(Ball,{coords,[{round(X - R),round(Y - R)},
			    {round(X + R),round(Y + R)}]}),
    zoom(Step-1.0,Ball,X-Xinc,Xinc,Y-Yinc,Yinc,R-Rinc,Rinc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Database processes holding holes or obstacles
%% Format [{Rec,X,A,B,C,D},...]
%% Rec = gs object, 
%% X = 1 for obstacle, 3 for hole (mouse button number)
%% (A,B) = upper left corner, (C,D) = lower right corner
%% Obstacles are stored half a ball (?Halfball = 20.0) larger 
%% than drawn to speed up the comparisons
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

black_holes() ->				% Function to give
    black_holes ! {get_holes,self()},		% the black hole list
    receive
	{holes,X} -> X
    end.

black_hole_process(X) ->		
    receive
	{get_holes,From} -> From ! X,
			    black_hole_process(X);
	{put_holes,Y} -> black_hole_process(Y);
	game_over -> exit(normal)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obstacles() ->					% Function to give
    obstacles ! {get_obs,self()},		% the obstacle list
    receive
	{obstacles,X} -> X
    end.

obstacle_process(X) ->
    receive
	{get_obs,From} -> From ! X,
			  obstacle_process(X);
	{put_obs,Y} -> obstacle_process(Y);
	game_over -> exit(normal)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

beep_process(Win) ->				% Beep process
    receive
	beep ->					% Beep on bump
	    gs:config(Win,{beep,true}),
	    receive after ?Timebeep -> true end,
	    flush_beep(),
	    beep_process(Win);
	game_over -> exit(normal)
    end.

flush_beep() ->					% Flush queued
    receive					% beep requests
	beep -> flush_beep()
    after 0 -> true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






