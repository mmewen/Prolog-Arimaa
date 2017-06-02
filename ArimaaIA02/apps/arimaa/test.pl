% ==== Tools ====
concat(L2,[],L2).
concat([X|RES],[X|L1],L2) :- concat(RES,L1,L2).

% Deletes empty items in a list
clear([], []).
clear(Res, [ [] | List ]) :- clear(Res, List).
clear([Item|Res], [ Item | List ]) :- clear(Res, List).

% ==== Facts ====

% Get the piece strength
strength(1, rabbit).
strength(2, cat).
strength(3, dog).
strength(4, horse).
strength(5, camel).
strength(6, elephant).
strength(S, [_, _, T, _]) :- strength(S, T).

% A piece is on a trap
is_on_trap([2, 2]).
is_on_trap([2, 5]).
is_on_trap([5, 2]).
is_on_trap([5, 5]).

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

% Tell if there is an opponent (and who is it exactly)
% Examples:
% 		- is_opponent(Who, [1, 1], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- is_opponent(Who, [7, 6], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- is_opponent(Who, [0, 1], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- opponent_in([[0,1], [2,1]], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- opponent_in([[0,2], [7,6]], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
is_opponent([R, C, T, gold], [Row, Col | _], Board) :- who_is_there([R, C, T, gold], [Row, Col], Board).
opponent_in([Position | Positions], Board) :- is_opponent(_, Position, Board); opponent_in(Positions, Board).

% Return all the pieces in the positions that are opponents
% Examples:
% 		- opponents_in(Opponents, [[0,1], [2,1]], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- opponents_in(Opponents, [[0,2], [7,6]], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
opponents_in([], [], _).
opponents_in([ Piece | Opponents ], [Pos | Positions], Board) :- is_opponent(Piece, Pos, Board), opponents_in(Opponents, Positions, Board).
opponents_in(Opponents, [Pos | Positions], Board) :- opponents_in(Opponents, Positions, Board).

% Tell if a piece is next to a friend (ie. same type)
% Examples:
% 		- next_to_friend([0,0,rabbit,silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- next_to_friend([6,7,rabbit,silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
next_to_friend([Row, Col, _, _], Board) :- fblr_positions(Positions, [Row, Col], null), friend_in(Positions, Board).

% Tell if a piece is next to an opponent (ie. different type)
% Examples:
% 		- next_to_opponent(Ops, [6,7,rabbit,silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
next_to_opponent(Opponents, [Row, Col, _, _], Board) :- fblr_positions(Positions, [Row, Col], null), opponents_in(Opponents, Positions, Board).

% P1 is strictly stronger than P2
is_stronger(P1, P2) :- strength(S1, P1), strength(S2, P2), S1 > S2.

% For all the pieces of the first list,
% true if at least one is stronger than the Piece
% Examples:
% 		- one_is_stronger([[7,6,horse,gold],[7,7,rabbit,gold]], [0,1,rabbit,silver]).
one_is_stronger([O | _], P) :- is_stronger(O, P).
one_is_stronger([O | Q], P) :- one_is_stronger(Q, P).

% True if is next to a stronger opponent
% Examples:
% 		- next_to_stronger_opponent([6,7,rabbit, silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- next_to_stronger_opponent([6,6,rabbit, silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
next_to_stronger_opponent(Piece, Board) :- next_to_opponent(Opponents, Piece, Board), one_is_stronger(Opponents, Piece).

% Tell if a piece is frozen
% Checks :
% 		- stronger neighbors
% 		- friendly pieces
% Examples:
% 		- is_frozen([6,7,rabbit, silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- is_frozen([6,6,rabbit, silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- is_frozen([6,6,rabbit, silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[5,6,dog,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
is_frozen(Piece, Board) :- next_to_stronger_opponent(Piece, Board), \+next_to_friend(Piece, Board).


% Well, the name is pretty explicit here...
% Checks :
% 		- pos is a trap
% 		- no close friend :/
% Examples:
% 		- is_on_trap_without_friend([2,2], [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ]], [[0,0,rabbit,silver],[0,1,rabbit,silver],[2,2,cat,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- is_on_trap_without_friend([2,2], [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ]], [[0,0,rabbit,silver],[2,1,rabbit,silver],[2,2,cat,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- is_on_trap_without_friend([0,0], [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ]], [[0,0,rabbit,silver],[2,1,rabbit,silver],[2,2,cat,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
is_on_trap_without_friend(Position, Gamestate, Board) :- is_on_trap(Position), fblr_positions(Positions, Position, _), \+friend_in(Positions, Board).

% true if there is noone on the position
% Examples :
%	- free_position([0,0],[[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
%	- free_position([1,0],[[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
free_position(_,[]).
free_position([Row,Col],[[Row,Col,_,_] | Board]) :- !,fail.
free_position(Position,[_ | Board]) :- free_position(Position,Board).

% Test if position in position2 are free
% Returns :
%	Positions : Positions2 without taken places
% Examples:
% 		- free_positions(Positions, [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]],[[0,1],[1,0],[3,0],[0,3]]).
list_empty([]).
free_positions([],_,[]).
free_positions([P|Positions],Board,[P|Positions2]) :- free_position(P,Board), free_positions(Positions,Board,Positions2).
free_positions(Positions,Board,[P|Positions2]) :- free_positions(Positions,Board,Positions2).

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
% Examples :
%	- possible_moves(Moves, [0,0,rabbit,silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[5,6,dog,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
%	- possible_moves(Moves, [6,6,rabbit, silver], [[0,0,rabbit,silver],[0,1,rabbit,silver],[6,6,rabbit, silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
%	- possible_moves(Moves, [0,1,rabbit,silver], [[0,0,rabbit,silver],[0,1,rabbit,silver]]).
possible_moves([], Piece, Board) :- is_frozen(Piece, Board).
possible_moves(Positions, [Row, Col, Type, _], Board) :- fblr_positions(Positions2, [Row, Col], Type),free_positions(Positions,Board,Positions2).

%Change format off possible_move return
possible_moves_to_tuple([],_,[]).
possible_moves_to_tuple([[Pos,M1_1]|M],Pos,[M1_1|M1]) :- possible_moves_to_tuple(M,Pos,M1).

% Return all possible 1 step moves for all the pieces of our color,
% for a gien Gamestate and Board
% B for recursivity
% Board  = full board
% WARNING : only works for silver pieces !
% Examples : 
%    - all_possible_moves(Moves,[[0,0,rabbit,silver],[0,1,rabbit,silver],[5,6,dog,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
%    - all_possible_moves(Moves,[[0,0,rabbit,silver],[0,1,rabbit,silver]]).
all_possible_moves([], [], _).
all_possible_moves(Moves, [[Row,Col,Type,silver]|B],Board) :- all_possible_moves(Moves1,B,Board),possible_moves(M1,[Row,Col,Type,silver],Board),possible_moves_to_tuple(M2,[Row,Col],M1),concat(Moves,Moves1,M2).
all_possible_moves(Moves, [[Row,Col,Type,_]|B],Board) :- all_possible_moves(Moves,B,Board).
all_possible_moves(Moves,Board) :- all_possible_moves(Moves,Board,Board).

%% --- ABOVE ARE TESTED

% Apply the given move to the given board
% don't remove
% Returns :
%	New gamestate and new board
apply_move(Gamestate2, Board3, [Origin|[NewRow,NewCol]], Gamestate1, Board1) :- who_is_there([Row,Col,Type,Color], Origin, Board1), replace([Row,Col,Type,Color], [NewRow,NewCol,Type,Color], Board1, Board2),apply_trap(Gamestate2,Board3,Position,Gamestate1,Board2).
apply_move(Gamestate1, Board3, [Origin|[NewRow,NewCol]], Gamestate1, Board1) :- who_is_there([Row,Col,Type,Color], Origin, Board1), replace([Row,Col,Type,Color], [NewRow,NewCol,Type,Color], Board1, Board2).


% Randomly pick one move out of all the Moves
% and change the Gamestate and Board
% TODO
one_random_move(M, Moves, Gamestate, Board).

% Move randomly one piece on the given board
% TODO
move_one(M, Gamestate2, Board2, Gamestate, Board) :- all_possible_moves(Moves, Gamestate, Board), one_random_move(M, Moves, Gamestate2, Board2).

% Take 4 random move in the possible moves and play it
% TODO
four_random_moves([M1, M2, M3, M4], Gamestate, Board) :- move_one(M1, Gamestate2, Board2, Gamestate, Board),
															move_one(M2, Gamestate3, Board3, Gamestate2, Board2),
															move_one(M3, Gamestate4, Board4, Gamestate3, Board3),
															move_one(M4, _, _, Gamestate4, Board4).

% Random get_moves
% TODO
get_moves(Moves, Gamestate, Board) :- four_random_moves(Moves, Gamestate, Board).