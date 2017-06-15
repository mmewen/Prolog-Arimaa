% ===== TOOLS =====

concat(L2,[],L2).
concat([X|RES],[X|L1],L2) :- concat(RES,L1,L2).

% Deletes empty items in a list
clear([], []).
clear(Res, [ [] | List ]) :- clear(Res, List).
clear([Item|Res], [ Item | List ]) :- clear(Res, List).

nth(M, 0, [M | _]).
nth(M, N, [_ | Moves]) :- N1 is N - 1, nth(M, N1, Moves).

% Replace copy board1 (3) in board2 (1) + new piece (4) - old piece (2)
% Replace3 copy board1 (3) in board2 (1) - old piece (2)
% Examples:
% 		- replace(Board2, [0, 1, rabbit, silver], [[0, 0, rabbit, silver], [0, 1, rabbit, silver]], [1, 1, rabbit, silver]).
replace_3([], _, []).
replace_3(B2, OldPiece, [OldPiece | B1]) :- replace_3(B2, OldPiece, B1).
replace_3([Piece | B2], OldPiece, [Piece | B1]) :- replace_3(B2, OldPiece, B1).
replace([[NewRow,NewCol,Type,Color] | Board2], OldPiece, Board1, [NewRow,NewCol,Type,Color]) :- replace_3(Board2, OldPiece, Board1).

% Returns true if the element M is in Ms.
% Also works in generation : it can return every element of Ms in M.
element(M, [M | _]).
element(M, [_ | Ms]):- element(M, Ms).



% ===== FACTS =====

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

goal([7,0]).
goal([7,1]).
goal([7,2]).
goal([7,3]).
goal([7,4]).
goal([7,5]).
goal([7,6]).
goal([7,7]).




% ===== BASIC PREDICATES =====

% Get pieces of a color still on the board
% Returns :
%	pieces : all the pieces of the specified Color
color_pieces([], [], _).
color_pieces([[Row, Col, Type, Color] | Pieces], [[Row, Col, Type, Color] | Board], Color) :- color_pieces(Pieces, Board, Color).
color_pieces(Pieces, [[_, _, _, _] | Board], Color) :- color_pieces(Pieces, Board, Color).

% Get position occupant
% who_is_there(Occupant, Position, Board)
% Examples:
% 		- who_is_there(O, [0, 0], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- who_is_there_array(O, [[1, 0], [0, 0]], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
who_is_there_array([], [], _).
who_is_there_array([O | Occupant], [[Row, Col] | Q], Board) :- who_is_there_array(Occupant, Q, Board), who_is_there(O, [Row, Col], Board).
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
opponents_in(Opponents, [_ | Positions], Board) :- opponents_in(Opponents, Positions, Board).

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
one_is_stronger([_ | Q], P) :- one_is_stronger(Q, P).

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
% 		- is_on_trap_without_friend([2,2], [[0,0,rabbit,silver],[0,1,rabbit,silver],[2,2,cat,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- is_on_trap_without_friend([2,2], [[0,0,rabbit,silver],[2,1,rabbit,silver],[2,2,cat,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
% 		- is_on_trap_without_friend([0,0], [[0,0,rabbit,silver],[2,1,rabbit,silver],[2,2,cat,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
is_on_trap_without_friend(Position, Board) :- is_on_trap(Position), fblr_positions(Positions, Position, null), \+friend_in(Positions, Board).

% true if there is noone on the position
% Examples :
%	- free_position([0,0],[[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
%	- free_position([1,0],[[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]]).
free_position(_,[]).
free_position([Row,Col],[[Row,Col,_,_] | _]) :- !,fail.
free_position(Position,[_ | Board]) :- free_position(Position,Board).

% Test if position in position2 are free
% Returns :
%	Positions : Positions2 without taken places
% Examples:
% 		- free_positions(Positions, [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,6,horse,gold],[7,7,rabbit,gold]],[[0,1],[1,0],[3,0],[0,3]]).
list_empty([]).
free_positions([],_,[]).
free_positions([P|Positions],Board,[P|Positions2]) :- free_position(P,Board), free_positions(Positions,Board,Positions2).
free_positions(Positions,Board,[_|Positions2]) :- free_positions(Positions,Board,Positions2).

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
%    - all_possible_moves(Moves,[[0,0,rabbit,silver],[2,2,rabbit,silver],[2,3,dog,gold]]).
all_possible_moves([], [], _).
all_possible_moves(Moves, [[Row,Col,Type,silver]|B],Board) :- all_possible_moves(Moves1,B,Board),possible_moves(M1,[Row,Col,Type,silver],Board),possible_moves_to_tuple(M2,[Row,Col],M1),concat(Moves,Moves1,M2).
all_possible_moves(Moves, [_|B],Board) :- all_possible_moves(Moves,B,Board).
all_possible_moves(Moves,Board) :- all_possible_moves(Moves,Board,Board).

% Return new Gamestate and Board or false
% Examples :
%    - apply_trap(Gamestate,Board,[],[[2,2,rabbit,silver]]).
%    - apply_trap(Gamestate,Board,[],[[1,2,cat,silver]]).
%    - apply_trap(Gamestate,Board,[],[[2,2,rabbit,silver],[1,2,cat,silver]]).
%    - apply_trap(Gamestate,Board,[],[[1,2,cat,silver],[2,2,rabbit,silver]]).
%    - apply_trap(Gamestate,Board,[],[[1,5,cat,silver],[2,2,rabbit,silver]]).
apply_trap_3(Gamestate,Board,Gamestate,[], Board).
apply_trap_3([[Row,Col,Type,Color]|Gamestate],Board2,Gamestate,[[Row,Col,Type,Color]|_], Board) :- is_on_trap_without_friend([Row,Col], Board), replace_3(Board2, [Row,Col,Type,Color], Board).
apply_trap_3(Gamestate2,B2,Gamestate,[_|B], Board) :- apply_trap_3(Gamestate2,B2,Gamestate,B, Board).
apply_trap(Gamestate2,B2,Gamestate,Board) :- apply_trap_3(Gamestate2,B2,Gamestate,Board, Board).

% Apply the given move to the given board
% don't remove
% Returns :
%	New gamestate and new board
% Examples :
%    - apply_move(Gamestate1, Board3, [[0, 1], [1, 1]], [], [[0,0,rabbit,silver], [0,1,rabbit,silver]]).
%    - apply_move(Gamestate1, Board3, [[2, 1], [2, 2]], [], [[0,0,rabbit,silver], [2,1,rabbit,silver]]).
%    - apply_move(Gamestate1, Board3, [[2, 1], [2, 2]], [], [[0,0,rabbit,silver], [2,1,rabbit,silver], [2,3,dog,silver]]).
apply_move(Gamestate2, Board3, [Origin, [NewRow,NewCol]], Gamestate1, Board1) :- who_is_there([Row,Col,Type,Color], Origin, Board1), replace(Board2, [Row,Col,Type,Color], Board1, [NewRow,NewCol,Type,Color]), apply_trap(Gamestate2, Board3, Gamestate1, Board2).




% ===== RANDOM MOVES =====

% Randomly pick one move out of all the Moves
% and change the Gamestate and Board
% Examples :
%    - one_random_move(M, [[[0, 1], [1, 1]], [[0, 1], [0, 2]], [[0, 0], [1, 0]]]).
one_random_move([], Moves) :- length(Moves,L), L = 0. % , print("ERROR : No move to pick at one_random_move").
one_random_move(M, Moves) :- length(Moves,L), random(0, L, N), nth(M, N, Moves).


% Move randomly one piece on the given board
% (M, G1, B1, [[[0, 1], [1, 1]], [[0, 1], [0, 2]], [[0, 0], [1, 0]]], [silver, [ [0,1,rabbit,silver], [0,2,horse,silver] ] ], [[0,0,rabbit,silver], [0,1,rabbit,silver], [7,6,horse,gold], [7,7,rabbit,gold]]).
% Examples :
%    - move_one_random(M, Gamestate2, Board2, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).
move_one_random(M, Gamestate2, Board2, Gamestate, Board) :- all_possible_moves(Moves, Board), one_random_move(M, Moves), apply_move(Gamestate2, Board2, M, Gamestate, Board).

% Take 4 random move in the possible moves and play it
% Examples :
%    - four_random_moves([M1, M2, M3, M4], [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).
four_random_moves([M1, M2, M3, M4], Gamestate, Board) :- move_one_random(M1, Gamestate2, Board2, Gamestate, Board),
															move_one_random(M2, Gamestate3, Board3, Gamestate2, Board2),
															move_one_random(M3, Gamestate4, Board4, Gamestate3, Board3),
															move_one_random(M4, _, _, Gamestate4, Board4).




% ===== SCORE =====

% Return distance between two positions
% Examples :
%    - get_dist(D,[6,3],[4,5]).
get_dist(D,[Row1,Col1],[Row2,Col2]) :- DR is Row1-Row2, DC is Col1-Col2, D is abs(DR)+abs(DC).

% Return the distance from a position to the goal (silver only)
% Examples :
%    - get_dist_from_goal(D,[5,3]).
%    - get_dist_from_goal(D,[7,1]).
get_dist_from_goal(D,[Row,_]) :- D is 7-Row.



%Return the closest position from a list and its distance for a given position
% Examples :
%    - get_closest(Pos,D,[[0,1]],[3,2]).
%    - get_closest(Pos,D,[[0,0],[5,4],[7,2],[1,6]],[3,2]).
get_closest_min(PosMin,DMin,PosMin,DMin,[],_).
get_closest_min(Pos,Dres,_,Dmin,[E|L],Origin) :- get_dist(D,Origin,E), D < Dmin, get_closest_min(Pos,Dres,E,D,L,Origin).
get_closest_min(Pos,D,PosMin,DMin,[_|L],Origin) :- get_closest_min(Pos,D,PosMin,DMin,L,Origin).
get_closest(Pos,D,[Einit|L],Origin) :-  get_dist(Dinit,Origin,Einit),get_closest_min(Pos,D,Einit,Dinit,L,Origin).


% return the distance of a position to the closest free goal (silver only)
% Examples :
%    - get_dist_to_freedom(S,[0,1],[[0,0,rabbit,silver],[0,1,rabbit,silver],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).
%    - get_dist_to_freedom(S,[2,3],[[7,2,rabbit,gold],[7,3,rabbit,gold],[7,4,rabbit,gold],[7,5,horse,gold],[7,7,rabbit,gold]]).
get_dist_to_freedom(S,[Row,Col],Board) :- setof(X, goal(X), L), free_positions(Positions,Board,L), get_closest(_,S,Positions,[Row,Col]).

%Erase if silver win with the given board
%Examples :
%    - win([[6,3,rabbit,silver],[4,6,rabbit,silver]]).
%    - win([[6,3,rabbit,silver],[7,6,rabbit,silver]]).
win([[7,_,rabbit,silver]|_]).
win([_|Board]) :- win(Board).

%% ---------------------------- IN THIS SECTION, ABOVE PREDICATES ARE TESTED ----------------------------

% Return the score of silver for the given gamestate
% Examples :
%    - get_taken_piece_score(S,[silver,[[0,0,rabbit,silver],[0,1,rabbit,silver],[0,5,elephant,silver],[7,5,rabbit,gold],[7,6,horse,gold]]]).
% TODO we could soustract when it's a gold piece
get_taken_piece_score(0,[]).
get_taken_piece_score(S,[[_,_,Type,silver] | List]) :- get_taken_piece_score(S1,List), strength(S2,Type), S is S1+S2.
get_taken_piece_score(S,[[_,_,_,gold] | List]) :- get_taken_piece_score(S,List).
get_taken_piece_score(S,[silver,List]) :- get_taken_piece_score(S,List).

% Return the score for a given board and gamestate
% if we want to do it for another color, add it in parameter
% Example :
%    - get_score(S,[silver,[[0,0,camel,silver],[0,1,dog,silver]]],[[2,3,rabbit,silver],[3,6,rabbit,silver],[7,2,rabbit,gold],[7,3,rabbit,gold],[7,4,rabbit,gold],[7,5,horse,gold],[7,7,rabbit,gold]]).
get_score(0,_,Board) :- win(Board).
get_score(S,Gamestate,Board) :- get_score2(S1,Board), get_taken_piece_score(S2,Gamestate), S is S1+S2*10.
get_score2(0,[]).
get_score2(S,[[Row,Col,rabbit,silver]|Board]) :- get_score2(S1,Board), get_dist_to_freedom(S2,[Row,Col],Board), get_dist_from_goal(S3,[Row,Col]), S is S1+S2+S3.
get_score2(S,[_|Board]) :- get_score2(S,Board).




% ===== BEST STATE AFTER X MOVES =====

% Take one move out of all the Moves.
% If called many times, will try every move given
% Examples :
%    - one_move(M, [[[0, 1], [1, 1]], [[0, 1], [0, 2]], [[0, 0], [1, 0]]]).
one_move(M, Moves) :- element(M, Moves).

% Apply the given moves to the given board.
% Not one after another ! Just try every movement given the current board
% Returns :
%	[ [Moves, New gamestate, new board], ...]
% Examples :
%    - apply_moves(MGBArray, [[[2, 1], [2, 2]], [[0, 0], [0, 1]], [[0, 0], [1, 0]]], [], [[0,0,rabbit,silver], [2,1,rabbit,silver], [2,3,dog,silver]]).
apply_moves([], [], _, _).
apply_moves([[[M], G, B] | StatesAfterMoves], [M | Moves], Gamestate, Board) :- apply_moves(StatesAfterMoves, Moves, Gamestate, Board),
	apply_move(G, B, M, Gamestate, Board).

% Sort the states according to their first argument (= their score)
% Examples :
%    - sort_states_by_score(SortedStates, [ [3, [[[2, 1], [2, 2]], [[0, 0], [0, 1]]], [], []], [5, [[[0, 0], [0, 1]], [[0, 0], [1, 0]]], [], []], [4, [[[2, 1], [2, 2]], [[0, 0], [0, 1]]], [], []] ]).
sort_states_by_score(SortedStates, ScoredStates) :- sort(1, @=<, ScoredStates, SortedStates).

% Take the state with the minimum score
% min_score([Score, Moves, NewGamestate, NewBoard], ScoredStatesAfterNMoves)
% Examples :
%    - min_score(BestState, [ [3, [[[2, 1], [2, 2]], [[0, 0], [0, 1]]], [], []], [5, [[[0, 0], [0, 1]], [[0, 0], [1, 0]]], [], []], [4, [[[2, 1], [2, 2]], [[0, 0], [0, 1]]], [], []] ]).
min_score(BestState, States) :- sort_states_by_score([BestState | _], States).

% Keep the Q first elements of the array of things
% or all the things if there are less than Q in it.
% Examples :
%    - keep_q_first(Kept, 5, [9, 8, 7, 6, 5, 4]).
%    - keep_q_first(Kept, 10, [9, 8, 7, 6, 5, 4]).
keep_q_first([], 0, _).
keep_q_first([], _, []).
keep_q_first([T | Kept], Q, [T | Things]) :- Q2 is Q - 1, keep_q_first(Kept, Q2, Things).

% Keep the Q best states of the list of scored states
% TODO: more efficient
% Examples :
%    - keep_q_best_scored_states(QBestStates, 2, [ [3, [[[2, 1], [2, 2]], [[0, 0], [0, 1]]], [], []], [5, [[[0, 0], [0, 1]], [[0, 0], [1, 0]]], [], []], [4, [[[2, 1], [2, 2]], [[0, 0], [0, 1]]], [], []] ]).
keep_q_best_scored_states(QBestStates, Q, ScoredStates) :- sort_states_by_score(SortedStates, ScoredStates), keep_q_first(QBestStates, Q, SortedStates).

% Get the score of all the given states
% get_states_score(ScoredStates, States)
%    - get_states_score(ScoredStates, [ [[], [silver,[[0,0,camel,silver],[0,1,dog,silver]]], [[2,3,rabbit,silver],[3,6,rabbit,silver],[7,2,rabbit,gold],[7,3,rabbit,gold],[7,4,rabbit,gold],[7,5,horse,gold],[7,7,rabbit,gold]]], [[], [silver,[[0,0,camel,silver],[0,1,dog,silver]]], [[7,1,rabbit,silver],[3,6,rabbit,silver],[7,2,rabbit,gold],[7,3,rabbit,gold],[7,4,rabbit,gold],[7,5,horse,gold],[7,7,rabbit,gold]]]]).
get_states_score([], []).
get_states_score([[Score, M, G, B] | ScoredStates], [[M, G, B] | States]) :- get_states_score(ScoredStates, States), get_score(Score, G, B).

% Examples :
%    - append_previous_moves(BestStates, [ [12,[[[2, 1], [2, 2]], [[0, 0], [0, 1]]], [silver,[[0,0,camel,silver]]], [[7,5,horse,gold],[7,7,rabbit,gold]]], [42, [[[0, 0], [0, 1]], [[0, 0], [1, 0]]], [silver,[[0,1,dog,silver]]], [[7,1,rabbit,silver]]] ], [[[2, 1], [2, 2]]]).
append_previous_moves([], [], _).
append_previous_moves([[S, Moves, G, B] | BestStates], [ [S, M, G, B] | BestStates2 ], PreviousMoves) :- append_previous_moves(BestStates, BestStates2, PreviousMoves), concat(Moves, PreviousMoves, M).

%% ---------------------------- IN THIS SECTION, ABOVE PREDICATES ARE TESTED ----------------------------

% Examples :
%    - explore_n_moves(BestStates, 0, 2, [ [12,[[[2, 1], [2, 2]], [[0, 0], [0, 1]]], [silver,[[0,0,camel,silver]]], [[7,5,horse,gold],[7,7,rabbit,gold]]] ]).
%    - explore_n_moves(BestStates, 1, 2, [ [12,[[[2, 1], [2, 2]], [[0, 0], [0, 1]]], [silver,[[0,0,camel,silver]]], [[5,4,horse,silver],[7,7,rabbit,gold]]], [42, [[[0, 0], [0, 1]], [[0, 0], [1, 0]]], [silver,[[0,1,dog,silver]]], [[6,1,rabbit,silver]]] ]).
%    - explore_n_moves(BestStates, 1, 2, [ [12,[[[2, 1], [2, 2]], [[0, 0], [0, 1]]], [silver,[[0,0,camel,silver]]], [[7,5,horse,gold],[7,7,rabbit,gold]]] ]).
explore_n_moves(States, 0, _, States).
explore_n_moves([], _, _, []).
explore_n_moves([BestStates | BestStates1], N, Q, [ [_, M, G, B] | StatesToExplore ]) :- explore_n_moves(BestStates1, N, Q, StatesToExplore),
	q_best_n_moves(BestStates2, N, Q, G, B),
	append_previous_moves(BestStates, BestStates2, M).

% From the given board, get all the possible 1 step moves
% Compute all the states [ MovesToGetToThisState, NewGamestate, NewBoard ] we would have if
% we apply each of the 1 step moves
% Compute the score of each state : [ StateScore, MovesToGetToThisState, NewGamestate, NewBoard ]
% (the smaller the better)
% Keep only the Q better states
% For each of these Q states, explore them (n-1 moves remaining)
% Return Q^N states
% Examples :
%    - q_best_n_moves(BestStates, 1, 2, [silver,[[0,0,camel,silver]]], [[5,4,horse,silver],[7,7,rabbit,gold]]).
q_best_n_moves([], 0, _, _, _).
q_best_n_moves(BestStates, N, Q, Gamestate, Board) :- all_possible_moves(Moves, Board),
	apply_moves(StatesAfterNMoves, Moves, Gamestate, Board), % !!!! TODO give the previous moves to put in the states moves !!
	get_states_score(ScoredStatesAfterNMoves, StatesAfterNMoves),
	keep_q_best_scored_states(QBestStates, Q, ScoredStatesAfterNMoves),
	N2 is N-1,
	explore_n_moves(BestStates, N2, Q, QBestStates).

% Get the Q best states possible from the current state in N moves,
% Compute their score
% Keep the best
% Play the corresponding moves% Examples :
%    - best_state(Moves, Gamestate, Board) 
best_state(Moves, Gamestate, Board) :- q_best_n_moves(StatesAfterNMoves, 4, 3, Gamestate, Board),
	get_states_score(ScoredStatesAfterNMoves, StatesAfterNMoves),
	min_score([Score, Moves, _, _], ScoredStatesAfterNMoves),
	print("Moves"),
	print(Moves),
	print("Score :"),
	print(Score).




% ===== GET MOVES =====

% Random get_moves
% Examples :
%    - get_moves([M1, M2, M3, M4], [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).
% get_moves(Moves, Gamestate, Board) :- four_random_moves(Moves, Gamestate, Board).

% Best state get_moves
get_moves(Moves, Gamestate, Board) :- best_state(Moves, Gamestate, Board).