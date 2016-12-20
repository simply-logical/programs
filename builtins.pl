%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%   Auxiliary Prolog programs for the book             %
%   SIMPLY LOGICAL: Intelligent reasoning by example   %
%   (c) Peter A. Flach/John Wiley & Sons, 1994.        %
%                                                      %
%   Predicates: not/1                                  %
%               '\='/2                                 %
%               forall/2                               %
%               varsin/2                               %
%                                                      %
%   These predicates are available in some Prologs,    %
%   but not in others (eg., in Quintus Prolog they     %
%   are not built-in, but available in libraries).     %
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-op(700,xfx,'\=').
:-op(900,fy,not).

not(X):-X,!,fail.
not(X).

X \= Y :- not X=Y.

forall(Goal,Condition):-
        bagof0(Condition,Goal,List),
        forall1(List).

forall1([]).
forall1([H|T]):-
        call(H),
        forall1(T).

bagof0(X,G,L):-
        bagof(X,G,L),!.
bagof0(X,G,[]).

varsin(Term,Vars):-
        varsin(Term,[],V),
        sort(V,Vars).

varsin(V,Vars,[V|Vars]):-
        var(V).
varsin(Term,V0,V):-
        functor(Term,F,N),
        varsin_args(N,Term,V0,V).

varsin_args(0,Term,Vars,Vars).
varsin_args(N,Term,V0,V):-
        N>0, N1 is N-1,
        arg(N,Term,ArgN),
        varsin(ArgN,V0,V1),
        varsin_args(N1,Term,V1,V).

