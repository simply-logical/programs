%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                      %
%   Prolog programs from Chapter 7 of the book         %
%   SIMPLY LOGICAL: Intelligent reasoning by example   %
%   (c) Peter A. Flach/John Wiley & Sons, 1994.        %
%                                                      %
%   Predicates: nl_shell/1                             %
%   and various Definite Clause Grammars               %
%                                                      %
%   NB. This file needs predicates defined in          %
%   the file 'library'.                                %
%                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-consult(library).


%%% 7.2  Definite Clause Grammars %%%

/*
%% Grammar with number agreement %%
sentence          --> noun_phrase(N),verb_phrase(N).
noun_phrase(N)    --> article(N),noun(N).
verb_phrase(N)    --> intransitive_verb(N).
article(singular) --> [a].
article(singular) --> [the].
article(plural)   --> [the].
noun(singular)    --> [turtle].
noun(plural)      --> [turtles].
intransitive_verb(singular) --> [sleeps].
intransitive_verb(plural)   --> [sleep].
*/

/*
%% Generating a parse tree %%
sentence(s(NP,VP))            --> noun_phrase(NP),verb_phrase(VP).
noun_phrase(np(N))            --> proper_noun(N).
noun_phrase(np(Art,Adj,N))    --> article(Art),adjective(Adj),noun(N).
noun_phrase(np(Art,N))        --> article(Art),noun(N).
verb_phrase(vp(IV))           --> intransitive_verb(IV).
verb_phrase(vp(TV,NP))        --> transitive_verb(TV),noun_phrase(NP).
article(art(the))             --> [the].
adjective(adj(lazy))          --> [lazy].
adjective(adj(rapid))         --> [rapid].
proper_noun(pn(achilles))     --> [achilles].
noun(n(turtle))               --> [turtle].
intransitive_verb(iv(sleeps)) --> [sleeps].
transitive_verb(tv(beats))    --> [beats].
*/

%% Numerals and numbers %%
numeral(N) --> n1_999(N).
numeral(N) --> n1_9(N1),[thousand],n1_999(N2),{N is N1*1000+N2}.

n1_999(N)  --> n1_99(N).
n1_999(N)  --> n1_9(N1),[hundred],n1_99(N2),{N is N1*100+N2}.

n1_99(N)   --> n0_9(N).
n1_99(N)   --> n10_19(N).
n1_99(N)   --> n20_90(N).
n1_99(N)   --> n20_90(N1),n1_9(N2),{N is N1+N2}.

n0_9(0)    --> [].
n0_9(N)    --> n1_9(N).

n1_9(1)    --> [one].
n1_9(2)    --> [two].
n1_9(3)    --> [three].
n1_9(4)    --> [four].
n1_9(5)    --> [five].
n1_9(6)    --> [six].
n1_9(7)    --> [seven].
n1_9(8)    --> [eight].
n1_9(9)    --> [nine].

n10_19(10) --> [ten].
n10_19(11) --> [eleven].
n10_19(12) --> [twelve].
n10_19(13) --> [thirteen].
n10_19(14) --> [fourteen].
n10_19(15) --> [fifteen].
n10_19(16) --> [sixteen].
n10_19(17) --> [seventeen].
n10_19(18) --> [eighteen].
n10_19(19) --> [nineteen].

n20_90(20) --> [twenty].
n20_90(30) --> [thirty].
n20_90(40) --> [fourty].
n20_90(50) --> [fifty].
n20_90(60) --> [sixty].
n20_90(70) --> [seventy].
n20_90(80) --> [eighty].
n20_90(90) --> [ninety].


%%% 7.3  Interpretation of natural language %%%

:-op(600,xfy,'=>').

sentence(C)  --> determiner(N,M1,M2,C),noun(N,M1),verb_phrase(N,M2).
sentence([(L:-true)])      --> proper_noun(N,X),verb_phrase(N,X=>L).
verb_phrase(s,M)           --> [is],property(s,M).
verb_phrase(p,M)           --> [are],property(p,M).
property(s,M)              --> [a],noun(s,M).
property(p,M)              --> noun(p,M).
property(N,X=>mortal(X))   --> [mortal].
determiner(s,X=>B,X=>H,[(H:-B)])                    --> [every].
determiner(p,sk=>H1,sk=>H2,[(H1:-true),(H2:-true)]) --> [some].
proper_noun(s,socrates)    --> [socrates].
noun(s,X=>human(X))        --> [human].
noun(p,X=>human(X))        --> [humans].
noun(s,X=>living_being(X)) --> [living],[being].
noun(p,X=>living_being(X)) --> [living],[beings].

question(Q)       --> [who],[is],property(s,X=>Q).
question(Q)       --> [is],proper_noun(N,X),property(N,X=>Q).
question((Q1,Q2)) --> [are],[some],noun(p,sk=>Q1),property(p,sk=>Q2).


% natural language shell
nl_shell(Rulebase):-
	get_input(Input),
	handle_input(Input,Rulebase).

handle_input(stop,Rulebase):-!.
handle_input(show,Rulebase):-!,
	show_rules(Rulebase),
	nl_shell(Rulebase).
handle_input(Sentence,Rulebase):-	% new rule
	phrase(sentence(Rule),Sentence),!,
	nl_shell([Rule|Rulebase]).
handle_input(Question,Rulebase):-	% question
	phrase(question(Query),Question),
	prove_rb(Query,Rulebase),!,
	transform(Query,Clauses),
	phrase(sentence(Clauses),Answer),
	show_answer(Answer),
	nl_shell(Rulebase).
handle_input(Question,Rulebase):- % illegal sentence or 
	show_answer('No'),               % no answer found
	nl_shell(Rulebase).

% show current rulebase
show_rules([]).
show_rules([Rule|Rules]):-
	phrase(sentence(Rule),Sentence),
	show_answer(Sentence),
	show_rules(Rules).

% meta-interpreter
prove_rb(true,Rulebase):-!.
prove_rb((A,B),Rulebase):-!,
	prove_rb(A,Rulebase),
	prove_rb(B,Rulebase).
prove_rb(A,Rulebase):-
	find_clause((A:-B),Rulebase),
	prove_rb(B,Rulebase).

% find applicable clause in rulebase
find_clause(Clause,[Rule|Rules]):-
	copy_element(Clause,Rule).	% don't instantiate Rule
find_clause(Clause,[Rule|Rules]):-
	find_clause(Clause,Rules).

%%% copy_element/2: see file 'library'

% transform query to answer
transform((A,B),[(A:-true)|Rest]):-!,
	transform(B,Rest).
transform(A,[(A:-true)]).

% get input from user
get_input(Input):-
	write('? '),read(Input).

% show answer to user
show_answer(Answer):-
	write('! '),write(Answer),nl.



