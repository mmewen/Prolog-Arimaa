:- module(bot,
      [  get_moves/3
      ]).
	
% A few comments but all is explained in README of github

% get_moves signature
% get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ]) 
% board: [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]

% Call exemple:
% get_moves(Moves, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

% default call
get_moves([[[1,0],[2,0]],[[0,0],[1,0]],[[0,1],[0,0]],[[0,0],[0,1]]], Gamestate, Board).


% Paste other predicates bellow this line
% ------------------------------------------------------------------

% ==== Tools ====
concat([],L2,L2).
concat([X|L1],L2,[X|RES]) :- concat(L1,L2,RES).

% Deletes empty items in a list
clear([], []).
clear(Res, [ [] | List ]) :- clear(Res, List).
clear([Item|Res], [ Item | List ]) :- clear(Res, List).

% Replace an element in a list by another one
% O : original term
% R : replace
% T : original list
% T2 : new list
replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).


% ==== Basic predicates ====

% Get pieces of a color still on the board
% Returns :
%	pieces : all the pieces of the specified Color
color_pieces([], [], _).
color_pieces([[Row, Col, Type, Color] | Pieces], [[Row, Col, Type, Color] | Board], Color) :- color_pieces(Pieces, Board, Color).
color_pieces(Pieces, [[_, _, _, _] | Board], Color) :- color_pieces(Pieces, Board, Color).

% Get position occupant
% who_is_there(Occupant, Position, Board)
who_is_there([], _, []).
who_is_there([Row, Col, Type, Color], [Row, Col], [[Row, Col, Type, Color] | _]).
who_is_there(Occupant, Position, [_ | Board]) :- who_is_there(Occupant, Position, Board).

% Get the valid position(s) on the board around the given position
% Examples:
% 		- fblr_positions(Pos, [7,7], rabbit).
% 		- fblr_positions(Pos, [7,7], truc).
% 		- fblr_positions(Pos, [5,5], truc).
back_position([], [0, _]).
back_position([Back, Col], [Row, Col]) :- Back is Row - 1.
front_position([], [7, _]).
front_position([Front, Col], [Row, Col]) :- Front is Row + 1.
left_position([], [_, 0]).
left_position([Row, Left], [Row, Col]) :- Left is Col - 1.
right_position([], [_, 7]).
right_position([Row, Right], [Row, Col]) :- Right is Col + 1.
fblr_positions(Positions, Position, rabbit) :- front_position(Front, Position), left_position(Left, Position), right_position(Right, Position), clear(Positions, [ Front, Left, Right ]).
fblr_positions(Positions, Position, _) :-  front_position(Front, Position), left_position(Left, Position), right_position(Right, Position), back_position(Back, Position), clear(Positions, [ Front, Left, Right, Back ]).

% Tell if there is a friend
% Examples:
% 		- is_friend([1, 1], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- is_friend([7, 6], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- is_friend([0, 1], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- friend_in([[0,1], [2,1]], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- friend_in([[0,2], [7,6]], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
is_friend([Row, Col], Board) :- who_is_there([_, _, _, silver], [Row, Col], Board).
friend_in([Pos | Positions], Board) :- is_friend(Pos, Board); friend_in(Positions, Board).

% Tell if there is an opponent
is_opponent([Row, Col], Board) :- who_is_there([_, _, _, gold], [Row, Col], Board).
opponent_in([Pos | Positions], Board) :- is_opponent(Pos, Board); is_opponent(Positions, Board).

% Tell if a piece is next to a friend (ie. same type)
% Examples:
% 		- next_to_friend([0,0,rabbit,silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- next_to_friend([6,7,rabbit,silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
next_to_friend([Row, Col, _, _], Board) :- fblr_positions(Positions, [Row, Col], null), friend_in(Positions, Board).

% Tell if a piece is next to an opponent (ie. different type)
% TODO : return opponents (we need their types)
% Examples:
% 		- next_to_opponent([6,7,rabbit,silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
next_to_opponent([Row, Col, _, _], Board) :- fblr_positions(Positions, [Row, Col], null), opponent_in(Positions, Board).

% Tell if a piece is frozen
% Checks :
% 		- stronger neighbors TODO
% 		- friendly pieces TODO
fblr_positions(Positions, Position, _) :-  front_position(Front, Position), left_position(Left, Position), right_position(Right, Position), back_position(Back, Position), clear(Positions, [ Front, Left, Right, Back ]).
is_frozen(Piece, Board) :- 

% Get all possible moves of a piece with a specific board
% WARNING : only works for silver pieces !
% Checks :
% 		- frozen pieces TODO
% 		- pull TODO
% 		- push TODO
%		- remaining moves
% 		- type (rabbits can't go backward)
% Returns :
%	Moves : a sub list of [ [i-1, j], [i, j+1], [i+1, j], [i, j-1] ]
possible_moves([Positions,[Row, Col]], [Row, Col, Type, silver], Board) :- fblr_positions(Positions, [Row, Col], Type).

% Return all possible 1 step moves for all the pieces of our color,
% for a gien Gamestate and Board
% WARNING : only works for silver pieces !
all_possible_moves([], []).
all_possible_moves([M|Moves], [[Row,Col,Type,silver]|Board]) :- all_possible_moves(Moves,Board),possible_moves(M,[Row,Col],Board).
all_possible_moves(Moves, [[Row,Col,Type,_]|Board]) :- all_possible_moves(Moves,Board).

% Checks if a piece if eaten by a trap 
% Returns :
%	New gamestate and new board
apply_trap([_,[Occupant|Gamestate2]],Board2,Position,Gamestate1,Board1) :- is_on_trap_without_friend(Position,Gamestate1,Board1), who_is_there(Occupant, Position, Board1), delete(Occupant,Board1,Board2).

% Apply the given move to the given board
% don't remove 
% Returns :
%	New gamestate and new board
apply_move(Gamestate2, Board3, [Origin|[NewRow,NewCol]], Gamestate1, Board1) :- who_is_there([Row,Col,Type,Color], Origin, Board1), replace([Row,Col,Type,Color], [NewRow,NewCol,Type,Color], Board1, Board2),apply_trap(Gamestate2,Board3,Position,Gamestate1,Board2).
apply_move(Gamestate1, Board3, [Origin|[NewRow,NewCol]], Gamestate1, Board1) :- who_is_there([Row,Col,Type,Color], Origin, Board1), replace([Row,Col,Type,Color], [NewRow,NewCol,Type,Color], Board1, Board2).

% Randomly pick one move out of all the Moves

one_random_move(M, Moves) :- nth0(N,Moves,M), N is random(L), length(Moves,L).

% Move randomly one piece on the given board
% and change the Gamestate and Board
move_one(M, Gamestate2, Board2, Gamestate, Board) :- all_possible_moves(Moves, Gamestate, Board), one_random_move(M, Moves), apply_move(Gamestate2, Board2, M, Gamestate, Board).

% Take 4 random move in the possible moves and play it
four_random_moves([M1, M2, M3, M4], Gamestate, Board) :- move_one(M1, Gamestate2, Board2, Gamestate, Board),
															move_one(M2, Gamestate3, Board3, Gamestate2, Board2),
															move_one(M3, Gamestate4, Board4, Gamestate3, Board3),
															move_one(M4, _, _, Gamestate4, Board4).

% Random get_moves
get_moves(Moves, Gamestate, Board) :- four_random_moves(Moves, Gamestate, Board).