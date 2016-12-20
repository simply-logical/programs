%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%   Prolog programs from Chapter 9 of the book         %
%   SIMPLY LOGICAL: Intelligent reasoning by example   %
%   (c) Peter A. Flach/John Wiley & Sons, 1994.        %
%                                                      %
%   Predicates: induce/2,3                             %
%               theta_subsumes/2                       %
%               anti_unify/3                           %
%               theta_lgg/3                            %
%                                                      %
%   NB. This file needs predicates defined in          %
%   the file 'library'.                                %
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-consult(library).


%% Naive induction %%
%% NB. The inductive meta-interpreter uses cl/2 rather than clause/2
%% in order to avoid problems with built-in predicate not/1

induce(E,H):-
	induce(E,[],H).

% induce(E,H0,H) <-	H is inductive explanation of E
induce(true,H,H).
induce((A,B),H0,H):-
	induce(A,H0,H1),
	induce(B,H1,H).
induce(A,H0,H):-
	cl(A,B),
	induce(B,H0,H).
induce(A,H0,H):-	% already assumed
	element((A:-B),H0),
	induce(B,H0,H).	% proceed with body of rule
induce(A,H0,[(A:-B)|H]):-	% A:-B can be added to the explanation
	inducible((A:-B)),	% if it's inducible
	not element((A:-B),H0),	% and if it's not already there
	induce(B,H0,H).	% proceed with body of rule

inducible((flies(X):-bird(X),has_feathers(X),has_beak(X))).
inducible((flies(X):-bird(X),has_feathers(X))).
inducible((flies(X):-bird(X),has_beak(X))).
inducible((flies(X):-has_feathers(X),has_beak(X))).
inducible((flies(X):-bird(X))).
inducible((flies(X):-has_feathers(X))).
inducible((flies(X):-has_beak(X))).
inducible((flies(X):-true)).


cl(bird(tweety),true).
cl(has_feathers(tweety),true).
cl(bird(polly),true).
cl(has_beak(polly),true).

% ?-induce(flies(tweety),H).
% ?-induce(flies(polly),H).


%%% 9.1  Generalisation and specialisation %%%

theta_subsumes((H1:-B1),(H2:-B2)):-
	not((H1=H2,ground(B2),
	     not subset(B1,B2))).

ground(Term):-
	numbervars(Term,0,N).


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

% ?-anti_unify(2*2=2+2,3*2=3+3,T,[],S1,[],S2).
% T = X*2=X+X
% S1 = [2<-X]
% S2 = [3<-X]



theta_lgg((H1:-B1),(H2:-B2),(H:-B)):-
	anti_unify(H1,H2,H,[],S10,[],S20),
	theta_lgg_bodies(B1,B2,[],B,S10,S1,S20,S2).

theta_lgg_bodies([],B2,B,B,S1,S1,S2,S2).
theta_lgg_bodies([L|B1],B2,B0,B,S10,S1,S20,S2):-
	theta_lgg_literal(L,B2,B0,B00,S10,S11,S20,S21),
	theta_lgg_bodies(B1,B2,B00,B,S11,S1,S21,S2).

theta_lgg_literal(L1,[],B,B,S1,S1,S2,S2).
theta_lgg_literal(L1,[L2|B2],B0,B,S10,S1,S20,S2):-
	same_predicate(L1,L2),
	anti_unify(L1,L2,L,S10,S11,S20,S21),
	theta_lgg_literal(L1,B2,[L|B0],B,S11,S1,S21,S2).
theta_lgg_literal(L1,[L2|B2],B0,B,S10,S1,S20,S2):-
	not same_predicate(L1,L2),
	theta_lgg_literal(L1,B2,B0,B,S10,S1,S20,S2).

%%% same_predicate/2: see file 'library'

% theta_lgg((element(c,[b,c]):-[element(c,[c])]),
%           (element(d,[b,c,d]):-[element(d,[c,d]),element(d,[d])]),
%           C).
% C = element(X,[b,c|Y]):-[element(X,[X]),element(X,[c|Y])]



