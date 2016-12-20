%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%   Prolog programs from Chapter 6 of the book         %
%   SIMPLY LOGICAL: Intelligent reasoning by example   %
%   (c) Peter A. Flach/John Wiley & Sons, 1994.        %
%                                                      %
%   Predicates: search_bstf/2                          %
%               tiles/2                                %
%               move/2                                 %
%               eval/2                                 %
%               search_beam/2,4                        %
%               search_hc/2                            %
%                                                      %
%   NB. This file needs predicates defined in          %
%   the file 'library'.                                %
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-consult(library).


%%% 6.1  Best-first search %%%

% best-first search
search_bstf([Goal|Rest],Goal):-
	goal(Goal).
search_bstf([Current|Rest],Goal):-
	children(Current,Children),
	add_bstf(Children,Rest,NewAgenda),
	search_bstf(NewAgenda,Goal).

% add_bstf(A,B,C) <- C contains the elements of A and B 
%                    (B and C sorted according to eval/2)
add_bstf([],Agenda,Agenda).
add_bstf([Child|Children],OldAgenda,NewAgenda):-
	add_one(Child,OldAgenda,TmpAgenda),
	add_bstf(Children,TmpAgenda,NewAgenda).

% add_one(S,A,B) <- B is A with S inserted acc. to eval/2
add_one(Child,OldAgenda,NewAgenda):-
	eval(Child,Value),
	add_one(Value,Child,OldAgenda,NewAgenda).
add_one(Value,Child,[],[Child]).
add_one(Value,Child,[Node|Rest],[Child,Node|Rest]):-
	eval(Node,V),
	Value<V.
add_one(Value,Child,[Node|Rest],[Node|NewRest]):-
	eval(Node,V),
	Value>=V,
	add_one(Value,Child,Rest,NewRest).


%% The sliding tiles puzzle %%

% tiles(M,C) <- moves M lead to a goal position at cost C 
%               (best-first search strategy)
tiles(Moves,Cost):-
	start(Start),
	eval(Start,Value),
	tiles_a([v(Value,Start)],Final,[],Visited),
	construct_moves(Final,Visited,[],Moves,0,Cost).

% tiles_a(A,M,V0,V) <- goal position can be reached from one of the
%                      positions on A with last move M (best-first)
%                      (V is list of visited nodes, V0 is accumulator)
tiles_a([v(V,LastMove)|Rest],LastMove,Visited,Visited):-
	goal(LastMove).
tiles_a([v(V,LastMove)|Rest],Goal,Visited0,Visited):-
	show_move(LastMove,V),
	setof0(v(Value,NextMove),
        (move(LastMove,NextMove),eval(NextMove,Value)),
        Children),
	merge(Children,Rest,NewAgenda),	% best-first
	tiles_a(NewAgenda,Goal,[LastMove|Visited0],Visited).

merge([],Agenda,Agenda).
merge([C|Cs],[],[C|Cs]).
merge([v(V1,Move1)|Rest1],[v(V2,Move2)|Rest2],[v(V1,Move1)|Rest3]):-
	V1<V2,
	merge(Rest1,[v(V2,Move2)|Rest2],Rest3).
merge([v(V1,Move1)|Rest1],[v(V2,Move2)|Rest2],[v(V2,Move2)|Rest3]):-
	V1>=V2,
	merge([v(V1,Move1)|Rest1],Rest2,Rest3).

% move(m(X,P,Y),m(P,NP,C)) <- position NP can be reached from current 
%                             position P in one move at cost C
move(m(OldPos,Pos,OldCost),m(Pos,NewPos,Cost)):-
	get_tile(Pos,Ne,e),get_tile(Pos,Nbw,BW),not(BW=e),
	D is Ne-Nbw,
	( D<0       -> Diff is -D
	; otherwise -> Diff is D ),	% Diff is abs(Ne-Nbw)
	Diff<4,
	replace(Pos,Ne,BW,Pos1),
	replace(Pos1,Nbw,e,NewPos),
	( Diff=1    -> Cost=1
	; otherwise -> Cost is Diff-1 ).

% reconstruct total cost and path from list of visited nodes
construct_moves(m(noparent,Start,0),Visited,Moves,[Start|Moves],Cost,Cost).
construct_moves(m(Parent,Pos,C),Visited,Moves0,Moves,Cost0,Cost):-
	element(m(GP,Parent,C1),Visited),	% GP is parent of Parent
	Cost1 is Cost0+C,
	construct_moves(m(GP,Parent,C1),Visited,[Pos|Moves0],Moves,Cost1,Cost).

start(m(noparent,[b,b,b,e,w,w,w],0)).

show_move(m(P,Pos,C),Value):-
	write(Pos-Value),nl.

get_tile(Pos,N,S):-
	get_tile(Pos,1,N,S).

get_tile([X|Xs],N,N,X).
get_tile([X|Xs],N0,N,Y):-
	N1 is N0+1,
	get_tile(Xs,N1,N,Y).

replace([X|Xs],1,Y,[Y|Xs]).
replace([X|Xs],N,Y,[X|Zs]):-
	N>1,N1 is N-1,
	replace(Xs,N1,Y,Zs).

goal(LastMove):-
	eval(LastMove,0).

eval(m(P,Pos,C),Value):-
%	bLeftOfw(Pos,Value).
	outOfPlace(Pos,1,0,Value).

bLeftOfw(Pos,Value):-
	findall(b,(get_tile(Pos,Nb,b),get_tile(Pos,Nw,w),Nb<Nw),L),
	length(L,Value).

outOfPlace(Pos,8,N,N).
outOfPlace(Pos,K,N0,N):-
	K<8, K1 is K+1,
	( K<4,get_tile(Pos,K,b) -> N1 is N0-(K-4)
	; K>4,get_tile(Pos,K,w) -> N1 is N0+(K-4)
	; otherwise -> N1=N0 ),
	outOfPlace(Pos,K1,N1,N).


%%% 6.3  Non-exhaustive informed search %%%

% beam search
search_beam(Agenda,Goal):-
	search_beam(1,Agenda,[],Goal).

search_beam(D,[],NextLayer,Goal):-
	D1 is D+1,
	search_beam(D1,NextLayer,[],Goal).
search_beam(D,[Goal|Rest],NextLayer,Goal):-
	goal(Goal).
search_beam(D,[Current|Rest],NextLayer,Goal):-
	children(Current,Children),
	add_beam(D,Children,NextLayer,NewNextLayer),
	search_beam(D,Rest,NewNextLayer,Goal).

% hill-climbing
search_hc(Goal,Goal):-
	goal(Goal).
search_hc(Current,Goal):-
	children(Current,Children),
	select_best(Children,Best),
	search_hc(Best,Goal).




