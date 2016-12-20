%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%   Prolog programs from Chapter 3 of the book         %
%   SIMPLY LOGICAL: Intelligent reasoning by example   %
%   (c) Peter A. Flach/John Wiley & Sons, 1994.        %
%                                                      %
%   Predicates: length/2                               %
%               length2/2,3                            %
%               naive_reverse/2                        %
%               reverse/2,3                            %
%               reverse_dl/3                           %
%               append/3                               %
%               prove/1                                %
%               prove_var/1                            %
%               prove_r/1                              %
%               prove_p/1,2                            %
%               write_proof/2                          %
%               partition/4                            %
%               sort/2                                 %
%               insert/3                               %
%   and various other small programs.                  %
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% 3.1  SLD resolution %%%

student_of(X,T):-follows(X,C),teaches(T,C).
follows(paul,computer_science).
follows(paul,expert_systems).
follows(maria,ai_techniques).
teaches(adrian,expert_systems).
teaches(peter,ai_techniques).
teaches(peter,computer_science).

brother_of(X,Y):-brother_of(Y,X).
brother_of(paul,peter).
brother_of(peter,adrian).
brother_of(X,Y):-brother_of(X,Z),brother_of(Z,Y).


%%% 3.1  Pruning the search by means of cut %%%

parent(X,Y):-father(X,Y),!.
parent(X,Y):-mother(X,Y).
father(john,paul).
father(john,peter).
mother(mary,paul).
mother(mary,peter).

likes(peter,Y):-friendly(Y).
likes(T,S):-student_of(S,T).
student_of(maria,peter).
student_of(paul,peter).
friendly(maria).


%%% 3.3  Negation as failure %%%

max(M,N,M):-M >= N.
max(M,N,N):-M =< N.

/*
% (incorrect) version with cut
max(M,N,M):-M >= N,!.
max(M,N,N).
*/

/*
not(Goal):-Goal,!,fail.
not(Goal).
*/

bachelor(X):-not(married(X)),man(X).
man(fred).
man(peter).
married(fred).


%%% 3.5  Arithmetic expressions %%%

nat(0).
nat(s(X)):-nat(X).

add(0,X,X).
add(s(X),Y,s(Z)):-add(X,Y,Z).

mul(0,X,0).
mul(s(X),Y,Z):-mul(X,Y,Z1),add(Y,Z1,Z).


%%% 3.6  Accumulators %%%

length([],0).
length([H|T],N):-length(T,M),N is M+1.

length2(L,N):-length2(L,0,N).

length2([],N,N).
length2([H|T],N0,N):-N1 is N0+1,length2(T,N1,N).

naive_reverse([],[]).
naive_reverse([H|T],R):-
	naive_reverse(T,R1),
	append(R1,[H],R).

append([],Y,Y).
append([H|T],Y,[H|Z]):-
	append(T,Y,Z).

reverse(X,Y):-
	reverse(X,[],Y).

reverse([],Y,Y).
reverse([H|T],Y0,Y):-
	reverse(T,[H|Y0],Y).

/*
reverse(X,Y):-
	reverse_dl(X,Y-[]).
*/

reverse_dl([],Y-Y).
reverse_dl([H|T],Y-Y0):-
	reverse_dl(T,Y-[H|Y0]).


%%% 3.7  Second-order predicates %%%

parents([],[]).
parents([P|Ps],[C|Cs]):-
	parent(P,C),
	parents(Ps,Cs).

rel(R,[],[]).
rel(R,[X|Xs],[Y|Ys]):-
	L =.. [R,X,Y],call(L),	% R(X,Y)
	rel(R,Xs,Ys).

parent(john,peter).
parent(john,paul).
parent(john,mary).
parent(mick,davy).
parent(mick,dee).
parent(mick,dozy).

children(Parent,Children):-
	findall(C,parent(Parent,C),Children).



%%% 3.8  Meta-programs %%%

% if A and B then C means if(then(and(A,B),C))
:-op(900,fx,if).
:-op(800,xfx,then).
:-op(700,yfx,and).

% object-level rules
if has_feathers and lays_eggs then is_bird.
if has_gills and lays_eggs then is_fish.
if tweety then has_feathers.
if tweety then lays_eggs.

% meta-program
derive(if Assumptions then Goal):-
	if Body then Goal,
	derive(if Assumptions then Body).
derive(if Assumptions then Goal1 and Goal2):-
	derive(if Assumptions then Goal1),
	derive(if Assumptions then Goal2).
derive(if Assumptions then Goal):-
	assumed(Goal,Assumptions).

assumed(A,A).
assumed(A,A and As).
assumed(A,B and As):-
	assumed(A,As).


prove(true):-!.
prove((A,B)):-!,
	prove(A),
	prove(B).
prove(A):-
	/* not A=true, not A=(X,Y) */
	clause(A,B),
	prove(B).

prove_var(true):-!.
prove_var((A,B)):-!,
	prove(A),
	prove(B).
prove_var(A):-
	clause(Head,Body),
	unify(A,Head,MGU,Result),
	apply(Body,MGU,NewBody),
	prove_var(NewBody).

is_bird(X):-has_feathers(X),lays_eggs(X).
is_fish(X):-has_gills(X),lays_eggs(X).
has_feathers(tweety).
lays_eggs(tweety).

% meta-interpreter with complete resolvent
prove_r(true):-!.
prove_r((A,B)):-!,
	clause(A,C),
	conj_append(C,B,D),
	prove_r(D).
prove_r(A):-
	clause(A,B),
	prove_r(B).
%%% conj_append/3: Utility predicate

% display a proof tree
prove_p(A):-
	prove_p(A,P),
	write_proof(P).

% prove_p(A,P) <- P is proof tree of A
prove_p(true,[]):-!.
prove_p((A,B),[p((A,B),Clause)|Proof]):-!,
	clause(A,C),
	copy_term((A:-C),Clause),	% make copy of the clause
	conj_append(C,B,D),
	prove_p(D,Proof).
prove_p(A,[p(A,(A:-B))|Proof]):-
	clause(A,B),
	prove_p(B,Proof).

write_proof([]):-
	tab(15),write('[]'),nl.
write_proof([p(A,B)|Proof]):-
	write((:-A)),nl,
	tab(5),write('|'),tab(10),write(B),nl,
	tab(5),write('|'),tab(20),write('/'),nl,
	write_proof(Proof).



%%% 3.9  A methodology of Prolog programming %%%

% partition(L,N,Littles,Bigs) <- Littles contains numbers 
%                                in L smaller than N, 
%                                Bigs contains the rest
partition([],N,[],[]).
partition([Head|Tail],N,[Head|Littles],Bigs):-
	Head < N,
	partition(Tail,N,Littles,Bigs).
partition([Head|Tail],N,Littles,[Head|Bigs]):-
	Head >= N,
	partition(Tail,N,Littles,Bigs).

sort([],[]).
sort([Head|Tail],WholeSorted):-
	sort(Tail,Sorted),
	insert(Head,Sorted,WholeSorted).

insert(X,[],[X]).
insert(X,[Head|Tail],[Head|Inserted]):-
	X > Head,
	insert(X,Tail,Inserted).
insert(X,[Head|Tail],[X,Head|Inserted]):-
	X =< Head.



