%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%   Prolog programs from Appendix A.2 of the book      %
%   SIMPLY LOGICAL: Intelligent reasoning by example   %
%   (c) Peter A. Flach/John Wiley & Sons, 1994.        %
%                                                      %
%   Predicates: element/2                              %
%               append/3                               %
%               remove_one/3                           %
%               subset/2                               %
%               proper_subset/2                        %
%               var_element/2                          %
%               var_remove_one/3                       %
%               var_proper_subset/2                    %
%               disj_element/2                         %
%               conj_append/3                          %
%               disj_append/3                          %
%               conj_remove_one/3                      %
%               copy_term/2                            %
%               copy_element/2                         %
%               try/1                                  %
%               setof0/3                               %
%               same_predicate/2                       %
%                                                      %
%   NB. In some Prologs, one or more of these          %
%   predicates may already be built-in. Such           %
%   built-in versions are typically more efficient.    %
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-consult(builtins).	% not needed if not/1, '\='/2, forall/2 and
			% varsin/2 are available in your Prolog


%%% Lists and sets

% element(X,Ys) <- X is an element of the list Ys
element(X,[X|Ys]).
element(X,[Y|Ys]):-
	element(X,Ys).

% append(Xs,Ys,Zs) <- list Zs is Xs followed by Ys
append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]):-
	append(Xs,Ys,Zs).

% remove_one(X,Ys,Zs) <- Zs is list Ys minus one occurrence of X
remove_one(X,[X|Ys],Ys).
remove_one(X,[Y|Ys],[Y|Zs]):-
	remove_one(X,Ys,Zs).

% subset(Xs,Ys) <- every element of list Xs occurs in list Ys
subset([],Ys).
subset([X|Xs],Ys):-
	element(X,Ys),
	subset(Xs,Ys).

% proper_subset(Xs,Ys) <- Xs is a subset of Ys, and Ys contains 
%                         at least one element more
proper_subset([],Ys):-
	Ys \= [].
proper_subset([X|Xs],Ys):-
	remove_one(X,Ys,Ys1),
	proper_subset(Xs,Ys1).

var_element(X,[Y|Ys]):-
	X == Y.	% syntactic identity
var_element(X,[Y|Ys]):-
	var_element(X,Ys).

var_remove_one(X,[Y|Ys],Ys):-
	X == Y.	% syntactic identity
var_remove_one(X,[Y|Ys],[Y|Zs]):-
	var_remove_one(X,Ys,Zs).

var_proper_subset([],Ys):-
	Ys \= [].
var_proper_subset([X|Xs],Ys):-
	var_remove_one(X,Ys,Zs),
	var_proper_subset(Xs,Zs).


%%% Conjunctions and disjunctions.

disj_element(X,X):-	% single-element disjunction
	not X=false,
	not X=(One;TheOther).
disj_element(X,(X;Ys)).
disj_element(X,(Y;Ys)):-
	disj_element(X,Ys).

conj_append(true,Ys,Ys).
conj_append(X,Ys,(X,Ys)):-	% single-element conjunction
	not X=true, 
	not X=(One,TheOther).
conj_append((X,Xs),Ys,(X,Zs)):-
	conj_append(Xs,Ys,Zs).

disj_append(false,Ys,Ys).
disj_append(X,Ys,(X;Ys)):-	% single-element disjunction
	not X=false, 
	not X=(One;TheOther).
disj_append((X;Xs),Ys,(X;Zs)):-
	disj_append(Xs,Ys,Zs).

conj_remove_one(X,X,true):-	% single-element conjunction
	not X=true,
	not X=(One,TheOther).
conj_remove_one(X,(X,Ys),Ys).
conj_remove_one(X,(Y,Ys),(Y,Zs)):-
	conj_remove_one(X,Ys,Zs).


%%% Preventing variables from getting instantiated.

% copy_term(Old,New) <- New is a copy of Old with new variables
copy_term(Old,New):-
	asserta('$copy'(Old)),
	retract('$copy'(New)),!.
copy_term(Old,New):-	% in case Old and New donÕt unify
	retract('$copy'(Old)),
	!,fail.

copy_element(X,Ys):-
	element(X1,Ys),
	copy_term(X1,X).

% try(Goal) <- Goal succeeds, but variables are not instantiated
try(Goal):-
	not not Goal.


%%% Various.

% variant of setof/3 which succeeds with the empty list
% if no solutions can be found
setof0(X,G,L):-
	setof(X,G,L),!.
setof0(X,G,[]).

% same_predicate(L1,L2) <- literals L1 and L2 have 
%                          the same predicate and arity
same_predicate(L1,L2):-
	functor(L1,P,N),functor(L2,P,N).

