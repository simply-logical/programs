%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%   Prolog programs from Chapter 5 of the book         %
%   SIMPLY LOGICAL: Intelligent reasoning by example   %
%   (c) Peter A. Flach/John Wiley & Sons, 1994.        %
%                                                      %
%   Predicates: search/2                               %
%               search_df/2                            %
%               search_df_loop/2                       %
%               search_bt/2                            %
%               search_d/3                             %
%               search_id/2,3                          %
%               search_bf/2                            %
%               prove_df/1                             %
%               prove_bf/1                             %
%               refute_bf/1                            %
%               model/1,2                              %
%               model_d/2,3                            %
%                                                      %
%   NB. This file needs predicates defined in          %
%   the file 'library'.                                %
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-consult(library).


%%% 5.1  A general search procedure %%%

% search(Agenda,Goal) <- Goal is a goal node, and a 
%                        descendant of one of the nodes 
%                        on the Agenda
search(Agenda,Goal):-
	next(Agenda,Goal,Rest),
	goal(Goal).
search(Agenda,Goal):-
	next(Agenda,Current,Rest),
	children(Current,Children),
	add(Children,Rest,NewAgenda),
	search(NewAgenda,Goal).


%%% 5.2  Depth-first search %%%

% depht-first search
search_df([Goal|Rest],Goal):-
	goal(Goal).
search_df([Current|Rest],Goal):-
	children(Current,Children),
	append(Children,Rest,NewAgenda),
	search_df(NewAgenda,Goal).

children(Node,Children):-
	findall(C,arc(Node,C),Children).

/*
children([Node|Path],Children):-
	findall([C,Node|Path],arc(Node,C),Children).
*/

% depth-first search with loop detection
search_df_loop([Goal|Rest],Visited,Goal):-
	goal(Goal).
search_df_loop([Current|Rest],Visited,Goal):-
	children(Current,Children),
	add(Children,Rest,Visited,NewAgenda),
	search_df_loop(NewAgenda,[Current|Visited],Goal).

add([],Agenda,Visited,Agenda).
add([Child|Rest],OldAgenda,Visited,[Child|NewAgenda]):-
	not element(Child,OldAgenda),
	not element(Child,Visited),
	add(Rest,OldAgenda,Visited,NewAgenda).
add([Child|Rest],OldAgenda,Visited,NewAgenda):-
	element(Child,OldAgenda),
	add(Rest,OldAgenda,Visited,NewAgenda).
add([Child|Rest],OldAgenda,Visited,NewAgenda):-
	element(Child,Visited),
	add(Rest,OldAgenda,Visited,NewAgenda).

% depth-first search by means of backtracking
search_bt(Goal,Goal):-
	goal(Goal).
search_bt(Current,Goal):-
	arc(Current,Child),
	search_bt(Child,Goal).

% backtracking depth-first search with depth bound
search_d(D,Goal,Goal):-
	goal(Goal).
search_d(D,Current,Goal):-
	D>0, D1 is D-1,
	arc(Current,Child),
	search_d(D1,Child,Goal).

% iterative deepening
search_id(First,Goal):-
	search_id(1,First,Goal).	% start with depth 1

search_id(D,Current,Goal):-
	search_d(D,Current,Goal).
search_id(D,Current,Goal):-
	D1 is D+1,	% increase depth
	search_id(D1,Current,Goal).


%%% 5.3  Breadth-first search %%%

% breadth-first search
search_bf([Goal|Rest],Goal):-
	goal(Goal).
search_bf([Current|Rest],Goal):-
	children(Current,Children),
	append(Rest,Children,NewAgenda),
	search_bf(NewAgenda,Goal).

% depth-first version of prove_r/1
prove_df(Goal):-
	prove_df_a([Goal]).

prove_df_a([true|Agenda]).
prove_df_a([(A,B)|Agenda]):-!,
	findall(D,(clause(A,C),conj_append(C,B,D)),Children),
	append(Children,Agenda,NewAgenda),
	prove_df_a(NewAgenda).
prove_df_a([A|Agenda]):-
	findall(B,clause(A,B),Children),
	append(Children,Agenda,NewAgenda),
	prove_df_a(NewAgenda).

likes(peter,Y):-student(Y),friendly(Y).
likes(X,Y):-friend(Y,X).
student(maria).
student(paul).
friendly(maria).
friend(paul,peter).

% breadth-first version of prove_r/1 + answer substitution
prove_bf(Goal):-
	prove_bf_a([a(Goal,Goal)],Goal).

prove_bf_a([a(true,Goal)|Agenda],Goal).
prove_bf_a([a((A,B),G)|Agenda],Goal):-!,
	findall(a(D,G),
	        (clause(A,C),conj_append(C,B,D)),
	        Children),
	append(Agenda,Children,NewAgenda),	% breadth-first
	prove_bf_a(NewAgenda,Goal).
prove_bf_a([a(A,G)|Agenda],Goal):-
	findall(a(B,G),clause(A,B),Children),
	append(Agenda,Children,NewAgenda),	% breadth-first
	prove_bf_a(NewAgenda,Goal).

% refute_bf(Clause) <- Clause is refuted by clauses 
%                      defined by cl/1 
%                      (breadth-first search strategy)
refute_bf(Clause):-
	refute_bf_a([a(Clause,Clause)],Clause).

refute_bf_a([a((false:-true),Clause)|Rest],Clause).
refute_bf_a([a(A,C)|Rest],Clause):-
	findall(a(R,C),(cl(Cl),resolve(A,Cl,R)),Children),
	append(Rest,Children,NewAgenda),	% breadth-first
	refute_bf_a(NewAgenda,Clause).

% resolve(C1,C2,R) <- R is the resolvent of C1 and C2.
resolve((H1:-B1),(H2:-B2),(ResHead:-ResBody)):-
	resolve(H1,B2,R1,R2),
	disj_append(R1,H2,ResHead),
	conj_append(B1,R2,ResBody).
resolve((H1:-B1),(H2:-B2),(ResHead:-ResBody)):-
	resolve(H2,B1,R2,R1),
	disj_append(H1,R2,ResHead),
	conj_append(R1,B2,ResBody).

resolve((A;B),C,B,E):-
	conj_remove_one(A,C,E).
resolve((A;B),C,(A;D),E):-
	resolve(B,C,D,E).
resolve(A,C,false,E):-
	conj_remove_one(A,C,E).

%%% disj_append/3, conj_remove_one/3 are in file 'library'

cl((bachelor(X);married(X):-man(X),adult(X))).
cl((has_wife(X):-man(X),married(X))).
cl((false:-has_wife(paul))).
cl((man(paul):-true)).
cl((adult(paul):-true)).


%%% 5.4  Forward chaining %%%

model(M):-
   model([],M).

model(M0,M):-
   is_violated(Head,M0),!,	% find instance of violated clause
   disj_element(L,Head),	% select ground literal from the head
   model([L|M0],M).	% and add it to the model
model(M,M).	% no more violated clauses

is_violated(H,M):-
   cl((H:-B)),
   satisfied_body(B,M),	% this will ground the variables
   not satisfied_head(H,M).

satisfied_body(true,M).	% body is a conjunction
satisfied_body(A,M):-
   element(A,M).
satisfied_body((A,B),M):-
   element(A,M),
   satisfied_body(B,M).

satisfied_head(A,M):-	% head is a disjunction
   element(A,M).
satisfied_head((A;B),M):-
   element(A,M).
satisfied_head((A;B),M):-
   satisfied_head(B,M).


model_d(D,M):-
	model_d(D,[],M).

model_d(0,M,M).
model_d(D,M0,M):-
	D>0,D1 is D-1,
	findall(H,is_violated(H,M0),Heads),
	satisfy_clauses(Heads,M0,M1),
	model_d(D1,M1,M).

satisfy_clauses([],M,M).
satisfy_clauses([H|Hs],M0,M):-
	disj_element(L,H),
	satisfy_clauses(Hs,[L|M0],M).


/*
cl((likes(peter,maria):-true)).
cl((student(maria):-true)).
cl((teacher(X);friendly(Y):-likes(X,Y),student(Y))).
cl((friendly(Y):-teacher(X),likes(X,Y))).
*/

/*
cl((man(X);woman(X):-true)).
cl((false:-man(maria))).
cl((false:-woman(peter))).
*/

/*
cl((man(X);woman(X):-person(X))).
cl((person(maria):-true)).
cl((person(peter):-true)).
cl((false:-man(maria))).
cl((false:-woman(peter))).
*/

/*
cl((append([],Y,Y):-list(Y))).
cl((append([X|Xs],Ys,[X|Zs]):-thing(X),append(Xs,Ys,Zs))).
cl((list([]):-true)).
cl((list([X|Y]):-thing(X),list(Y))).
cl((thing(a):-true)).
cl((thing(b):-true)).
cl((thing(c):-true)).
*/



