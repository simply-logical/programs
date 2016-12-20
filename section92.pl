%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%   Prolog programs from Section 9.2 of the book       %
%   SIMPLY LOGICAL: Intelligent reasoning by example   %
%   (c) Peter A. Flach/John Wiley & Sons, 1994.        %
%                                                      %
%   Predicates: induce_rlgg/2                          %
%               rlgg/4                                 %
%                                                      %
%   NB. This file needs predicates defined in          %
%   the file 'library'.                                %
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-consult(library).


%%% 9.2  Bottom-up induction %%%

induce_rlgg(Exs,Clauses):-
	pos_neg(Exs,Poss,Negs),
	bg_model(BG),
	append(Poss,BG,Model),
	induce_rlgg(Poss,Negs,Model,Clauses).

induce_rlgg(Poss,Negs,Model,Clauses):-
	covering(Poss,Negs,Model,[],Clauses).

% split positive and negative examples
pos_neg([],[],[]).
pos_neg([+E|Exs],[E|Poss],Negs):-
	pos_neg(Exs,Poss,Negs).
pos_neg([-E|Exs],Poss,[E|Negs]):-
	pos_neg(Exs,Poss,Negs).

% covering algorithm
covering(Poss,Negs,Model,H0,H):-
	construct_hypothesis(Poss,Negs,Model,Hyp),!,
	remove_pos(Poss,Model,Hyp,NewPoss),
	covering(NewPoss,Negs,Model,[Hyp|H0],H).
covering(P,N,M,H0,H):-
	append(H0,P,H).	% add uncovered examples to hypothesis

% remove covered positive examples
remove_pos([],M,H,[]).
remove_pos([P|Ps],Model,Hyp,NewP):-
	covers_ex(Hyp,P,Model),!,
	write('Covered example: '),write(P),nl,
	remove_pos(Ps,Model,Hyp,NewP).
remove_pos([P|Ps],Model,Hyp,[P|NewP]):-
	remove_pos(Ps,Model,Hyp,NewP).


% extensional coverage, relative to a ground model
covers_ex((Head:-Body),Example,Model):-
	try((Head=Example,forall(element(L,Body),element(L,Model)))).

% construct a clause by means of RLGG
construct_hypothesis([E1,E2|Es],Negs,Model,Clause):-
	write('RLGG of '),write(E1),write(' and '),write(E2),write(' is'),
	rlgg(E1,E2,Model,Cl),
	reduce(Cl,Negs,Model,Clause),!,
	nl,tab(5),write(Clause),nl.
construct_hypothesis([E1,E2|Es],Negs,Model,Clause):-
	write(' too general'),nl,
	construct_hypothesis([E2|Es],Negs,Model,Clause).


% rlgg(E1,E2,M,C) <- C is RLGG of E1 and E2 relative to M
rlgg(E1,E2,M,(H:-B)):-
	anti_unify(E1,E2,H,[],S10,[],S20),
	varsin(H,V),	% determine variables in head of clause
	rlgg_bodies(M,M,[],B,S10,S1,S20,S2,V).

rlgg_bodies([],B2,B,B,S1,S1,S2,S2,V).
rlgg_bodies([L|B1],B2,B0,B,S10,S1,S20,S2,V):-
	rlgg_literal(L,B2,B0,B00,S10,S11,S20,S21,V),
	rlgg_bodies(B1,B2,B00,B,S11,S1,S21,S2,V).

rlgg_literal(L1,[],B,B,S1,S1,S2,S2,V).
rlgg_literal(L1,[L2|B2],B0,B,S10,S1,S20,S2,V):-
	same_predicate(L1,L2),
	anti_unify(L1,L2,L,S10,S11,S20,S21),
 varsin(L,Vars),var_proper_subset(Vars,V),	% no new variables in literal
	!,rlgg_literal(L1,B2,[L|B0],B,S11,S1,S21,S2,V).
rlgg_literal(L1,[L2|B2],B0,B,S10,S1,S20,S2,V):-
	rlgg_literal(L1,B2,B0,B,S10,S1,S20,S2,V).


:-op(600,xfx,'<-').

anti_unify(Term1,Term2,Term):-
	anti_unify(Term1,Term2,Term,[],S1,[],S2).

anti_unify(Term1,Term2,Term1,S1,S1,S2,S2):-
	Term1 == Term2,!.
anti_unify(Term1,Term2,V,S1,S1,S2,S2):-
	subs_lookup(S1,S2,Term1,Term2,V),!.
anti_unify(Term1,Term2,Term,S10,S1,S20,S2):-
	nonvar(Term1),nonvar(Term2),
	functor(Term1,F,N),functor(Term2,F,N),!,
	functor(Term,F,N),
	anti_unify_args(N,Term1,Term2,Term,S10,S1,S20,S2).
anti_unify(Term1,Term2,V,S10,[Term1<-V|S10],S20,[Term2<-V|S20]).

anti_unify_args(0,Term1,Term2,Term,S1,S1,S2,S2).
anti_unify_args(N,Term1,Term2,Term,S10,S1,S20,S2):-
	N>0,N1 is N-1,
	arg(N,Term1,Arg1),
	arg(N,Term2,Arg2),
	arg(N,Term,Arg),
	anti_unify(Arg1,Arg2,Arg,S10,S11,S20,S21),
	anti_unify_args(N1,Term1,Term2,Term,S11,S1,S21,S2).

subs_lookup([T1<-V|Subs1],[T2<-V|Subs2],Term1,Term2,V):-
	T1 == Term1,
	T2 == Term2,!.
subs_lookup([S1|Subs1],[S2|Subs2],Term1,Term2,V):-
	subs_lookup(Subs1,Subs2,Term1,Term2,V).


% remove redundant literals
reduce((H:-B0),Negs,M,(H:-B)):-
	setof0(L,(element(L,B0),not var_element(L,M)),B1),
	reduce_negs(H,B1,[],B,Negs,M).

% reduce_negs(H,B1,B0,B,N,M) <- B is a subsequence of B1
%                               such that H:-B does not
%                               cover elements of N
reduce_negs(H,[L|B0],In,B,Negs,M):-
	append(In,B0,Body),
	not covers_neg((H:-Body),Negs,M,N),!,
	reduce_negs(H,B0,In,B,Negs,M).
reduce_negs(H,[L|B0],In,B,Negs,M):-
	reduce_negs(H,B0,[L|In],B,Negs,M).
reduce_negs(H,[],Body,Body,Negs,M):-
	not covers_neg((H:-Body),Negs,M,N).

covers_neg(Clause,Negs,Model,N):-
	element(N,Negs),
	covers_ex(Clause,N,Model).


%%% Queries %%%

%%%%%%%%%%%%%%%%%%  element/2  %%%%%%%%%%%%%%%%%%%%%%%%

% bg_model([]).

query1(Clauses):-
	induce_rlgg([+element(b,[b]),
	             +element(2,[2,3]),
	             +element(3,[1,2,3]),
	             +element(b,[a,b]),
	             +element(3,[2,3]),
	             +element(3,[3]),
	             -element(3,[a,b]),
	             -element(a,[])
	            ],Clauses).

%%%%%%%%%%%%%%%%%%  append/3   %%%%%%%%%%%%%%%%%%%%%%%

% bg_model([]).

query2(Clauses):-
	induce_rlgg([+append([1,2],[3,4],[1,2,3,4]),
	             +append([a],[],[a]),
	             +append([],[],[]),
	             +append([],[1,2,3],[1,2,3]),
	             +append([2],[3,4],[2,3,4]),
	             +append([],[3,4],[3,4]),
	             -append([a],[b],[b]),
	             -append([c],[b],[c,a]),
	             -append([1,2],[],[1,3])
	            ],Clauses).

%%%%%%%%%%%%%%%%%%   num/2    %%%%%%%%%%%%%%%%%%%%%%%

bg_model([num(1,one),
	         num(2,two),
	         num(3,three),
	         num(4,four),
	         num(5,five)
	        ]).

query3(Clauses):-
	induce_rlgg([+listnum([],[]),
	             +listnum([2,three,4],[two,3,four]),
	             +listnum([4],[four]),
	             +listnum([three,4],[3,four]),
	             +listnum([two],[2]),
	             -listnum([1,4],[1,four]),
	             -listnum([2,three,4],[two]),
	             -listnum([five],[5,5])
	            ],Clauses).

%%%%%%%%%%%%%%%%%%   names/2    %%%%%%%%%%%%%%%%%%%%%%%

/*
bg_model([person(mick,jagger),
	         person(david,bowie),
	         person(tina,turner),
	         person(johann,sebastian),
	         person(ludwig,van)
	        ]).
*/

query4(Clauses):-
	induce_rlgg([+names([],[]),
	             +names([david,turner,johann],
																					[p(david,bowie),p(tina,turner),p(johann,sebastian)]),
	             +names([johann],[p(johann,sebastian)]),
	             +names([turner,johann],[p(tina,turner),p(johann,sebastian)]),
	             +names([bowie],[p(david,bowie)]),
	             -names([mick,johann],[p(mick,mick),p(johann,sebastian)]),
	             -names([david,turner,johann],[p(david,bowie)]),
	             -names([van],[p(ludwig,van),p(ludwig,van)])
	            ],Clauses).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




