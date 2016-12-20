%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%   Prolog programs from Chapter 8 of the book         %
%   SIMPLY LOGICAL: Intelligent reasoning by example   %
%   (c) Peter A. Flach/John Wiley & Sons, 1994.        %
%                                                      %
%   Predicates: explain/2                              %
%               abduce/2,3   (several versions)        %
%               min_diagnosis/2                        %
%               diagnosis/2                            %
%                                                      %
%   NB. This file needs predicates defined in          %
%   the file 'library'.                                %
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-consult(library).


%%% 8.1  Default reasoning %%%

explain(A,E):-
  explain(A,[],E).

explain(true,E,E):-!.
explain((A,B),E0,E):-!,
  explain(A,E0,E1),
  explain(B,E1,E).
explain(A,E0,E):-
  prove_e(A,E0,E).	% explain by facts only
explain(A,E0,[default(Name)|E]):-
  default(Name,(A:-B)),	% explain by default rule
  explain(B,E0,E),
  not contradiction(Name,E),	% default should be applicable
  not contradiction(A,E).	% A should be consistent with E

prove_e(true,E,E):-!.
prove_e((A,B),E0,E):-!,
  prove_e(A,E0,E1),
  prove_e(B,E1,E).
prove_e(A,E0,[rule((A:-B))|E]):-
  rule((A:-B)),
  prove_e(B,E0,E).

contradiction(not A,E):-!,
  prove_e(A,E,E1).
contradiction(A,E):-
  prove_e(not A,E,E1).


default(birdsfly(X),(flies(X):-bird(X))).

rule((bird(X):-penguin(X))).
rule((not flies(X):-penguin(X))).
rule((bird(polly):-true)).
rule((penguin(tweety):-true)).

default(mammals_dont_fly(X),(not flies(X):-mammal(X))).
default(bats_fly(X),(flies(X):-bat(X))).
default(dead_things_dont_fly(X),(not flies(X):-dead(X))).

rule((mammal(X):-bat(X))).
rule((bat(dracula):-true)).
rule((dead(dracula):-true)).
rule((not mammals_dont_fly(X):-bat(X))).
rule((not bats_fly(X):-dead(X))).


%%% 8.3  Abduction and diagnostic reasoning %%%

%% Abduction %%
%% NB. The abductive meta-interpreter uses cl/2 rather than clause/2
%% in order to avoid problems with built-in predicate not/1

abduce(O,E):-
	abduce(O,[],E).

/*
% abduce(O,E0,E) <-	E is abductive explanation of O
% first version, for definite clauses
abduce(true,E,E):-!.
abduce((A,B),E0,E):-!,
	abduce(A,E0,E1),
	abduce(B,E1,E).
abduce(A,E0,E):-
	cl(A,B),
	abduce(B,E0,E).
abduce(A,E,E):-	% already assumed
	element(A,E).
abduce(A,E,[A|E]):-	% A can be added to E
	not element(A,E),	% if it's not already there,
	abducible(A).	% and if it's abducible
*/

/*
% abduce(O,E0,E) <-	E is abductive explanation of O
% second version, for normal clauses (incorrect)
abduce(true,E,E):-!.
abduce((A,B),E0,E):-!,
	abduce(A,E0,E1),
	abduce(B,E1,E).
abduce(A,E0,E):-
	cl(A,B),
	abduce(B,E0,E).
abduce(A,E,E):-	% already assumed
	element(A,E).
abduce(A,E,[A|E]):-	% A can be added to E
	not element(A,E),	% if it's not already there,
	abducible(A).	% and if it's abducible
abduce(not(A),E,E):-	% E explains not(A)
	not abduce(A,E,E).	% if E doesn't explain A
*/

% abduce(O,E0,E) <-	E is abductive explanation of O
% third version, for normal clauses (correct)
abduce(true,E,E):-!.
abduce((A,B),E0,E):-!,
	abduce(A,E0,E1),
	abduce(B,E1,E).
abduce(A,E0,E):-
	cl(A,B),
	abduce(B,E0,E).
%	abduce(B,[A|E0],E).
abduce(A,E,E):-
	element(A,E).	% already assumed
abduce(A,E,[A|E]):-	% A can be added to E
	not element(A,E),	% if it's not already there,
	abducible(A),	% if it's abducible,
	not abduce_not(A,E,E).	% and E doesn't explain not(A)
abduce(not(A),E0,E):-	% find explanation for not(A)
	not element(A,E0),	% should be consistent
	abduce_not(A,E0,E).

% abduce_not(O,E0,E) <-	E is abductive explanation of not(O)
abduce_not((A,B),E0,E):-!,
	abduce_not(A,E0,E);	% disjunction
	abduce_not(B,E0,E).
abduce_not(A,E0,E):-
	setof(B,cl(A,B),L),
	abduce_not_l(L,E0,E).
%	abduce_not_l(L,[not(A)|E0],E).
abduce_not(A,E,E):-
	element(not(A),E).	% not(A) already assumed
abduce_not(A,E,[not(A)|E]):-	% not(A) can be added to E
	not element(not(A),E),	% if it's not already there,
	abducible(A),	% if A is abducible
	not abduce(A,E,E).	% and E doesn't explain A
abduce_not(not(A),E0,E):-	% find explanation for A
	not element(not(A),E0),	% should be consistent
	abduce(A,E0,E).

abduce_not_l([],E,E).
abduce_not_l([B|Bs],E0,E):-
	abduce_not(B,E0,E1),
	abduce_not_l(Bs,E1,E).

abducible(A):-
	A \= not(X),
	not cl(A,B).


cl(likes(peter,S),student_of(S,peter)).
cl(likes(X,Y),friend(Y,X)).

% ?-abduce(likes(peter,maria),E).
% E = [student_of(maria,peter)]
% E = [friend(maria,peter)]


cl(flies(X),(bird(X),not(abnormal(X)))).
cl(abnormal(X),penguin(X)).
cl(abnormal(X),dead(X)).
cl(bird(X),penguin(X)).
cl(bird(X),sparrow(X)).

% ?-abduce(flies(tweety),E).
% E = [not penguin(tweety),not dead(tweety),sparrow(tweety)]


cl(flies1(X),(not(abnormal(X)),bird(X))).

% ?-abduce(flies1(tweety),E).
% E = [sparrow(tweety),not penguin(tweety),not dead(tweety)]


%% Diagnosis %%

cl(add3bit(N,X1,Y1,X2,Y2,X3,Y3,Sum1,Sum2,Sum3),
  (adder(N-add1,X1,Y1,0,Sum1,Carry1),
   adder(N-add2,X2,Y2,Carry1,Sum2,Carry2),
   adder(N-add3,X3,Y3,Carry2,Sum3,0))).

cl(adder(N,X,Y,Z,Sum,Carry),
  (xorg(N-xor1,X,Y,S),
   xorg(N-xor2,Z,S,Sum),
   andg(N-and1,X,Y,C1),
   andg(N-and2,Z,S,C2),
   org(N-or1,C1,C2,Carry))).


cl(xorg(N,X,Y,Z),xor(X,Y,Z)).
cl(xorg(N,1,1,1),fault(N=s1)).
cl(xorg(N,0,0,1),fault(N=s1)).
cl(xorg(N,1,0,0),fault(N=s0)).
cl(xorg(N,0,1,0),fault(N=s0)).

cl(andg(N,X,Y,Z),and(X,Y,Z)).
cl(andg(N,0,0,1),fault(N=s1)).
cl(andg(N,1,0,1),fault(N=s1)).
cl(andg(N,0,1,1),fault(N=s1)).
cl(andg(N,1,1,0),fault(N=s0)).

cl(org(N,X,Y,Z),or(X,Y,Z)).
cl(org(N,0,0,1),fault(N=s1)).
cl(org(N,1,0,0),fault(N=s0)).
cl(org(N,0,1,0),fault(N=s0)).
cl(org(N,1,1,0),fault(N=s0)).


cl(xor(1,0,1),true).
cl(xor(0,1,1),true).
cl(xor(1,1,0),true).
cl(xor(0,0,0),true).

cl(and(1,1,1),true).
cl(and(1,0,0),true).
cl(and(0,1,0),true).
cl(and(0,0,0),true).

cl(or(1,1,1),true).
cl(or(1,0,1),true).
cl(or(0,1,1),true).
cl(or(0,0,0),true).


% abducible(fault(X)).


min_diagnosis(O,D):-
	diagnosis(O,D),
	not((diagnosis(O,D1),proper_subset(D1,D))).

diagnosis(Observation,Diagnosis):-
	abduce(Observation,Diagnosis).

%%% proper_subset/2: see file 'library'

/*
?-diagnosis(adder(a,0,0,1,0,1),D).
D = [fault(a-or1=s1),fault(a-xor2=s0)]
D = [fault(a-and2=s1),fault(a-xor2=s0)]
D = [fault(a-and1=s1),fault(a-xor2=s0)]
D = [fault(a-and2=s1),fault(a-and1=s1),fault(a-xor2=s0)]
D = [fault(a-xor1=s1)]
D = [fault(a-or1=s1),fault(a-and2=s0),fault(a-xor1=s1)]
D = [fault(a-and1=s1),fault(a-xor1=s1)]
D = [fault(a-and2=s0),fault(a-and1=s1),fault(a-xor1=s1)]
No more solutions

?-min_diagnosis(adder(a,0,0,1,0,1),D).
D = [fault(a-or1=s1),fault(a-xor2=s0)]
D = [fault(a-and2=s1),fault(a-xor2=s0)]
D = [fault(a-and1=s1),fault(a-xor2=s0)]
D = [fault(a-xor1=s1)]
No more solutions
*/



%%% 8.4  The complete picture %%%

notflies(X):-mammal(X),not flying_mammal(X).
flies(X):-bat(X),not nonflying_bat(X).
notflies(X):-dead(X),not flying_deadthing(X).

mammal(X):-bat(X).
bat(dracula).
dead(dracula).
flying_mammal(X):-bat(X).
nonflying_bat(X):-dead(X).
flying_deadthing.


/*
cl(notflies(X),(mammal(X),not flying_mammal(X))).
cl(flies(X),(bat(X),not nonflying_bat(X))).
cl(notflies(X),(dead(X),not flying_deadthing(X))).

cl(mammal(X),bat(X)).
cl(bat(dracula),true).
cl(dead(dracula),true).
cl(flying_mammal(X),bat(X)).
cl(nonflying_bat(X),dead(X)).

abducible(flying_mammal(X)).
abducible(nonflying_bat(X)).
abducible(flying_deadthing(X)).

% ?-abduce(flies(X),E).
% no

% ?-abduce(notflies(X),E).
% X = dracula
% E = [not flying_deadthing(dracula)]
*/

