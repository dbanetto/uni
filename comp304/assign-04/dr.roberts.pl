%% Questions and expected outputs
question([i, fantasised, about, fast, cars]).
% expected: [have, you, ever, fantasised, about, fast, cars, before, qm]
question([i, feel, bad, about, my, brother]).
% expected: [what, makes, you, feel, bad, about, your, brother, qm] 
question([i, know, i, am, insecure]). 
% expected:  [are, you, sure, you, know, that, you, are, insecure, qm]
question([i, like, my, mother]). 
% expected:  [why, you, like, your, mother, qm]

%% printSentence, expects a list
% if it is an empty list we know we are at the end of the sentence so put a new line
% to make the sentence looks nice
printSentance([]):- write("\n"). 
% write each word one by one recursively using
% writeWord to do special formatting for words like `qm`.
printSentance(Sentence):- 
    Sentence = [Word | Rest], 
    writeWord(Word),
    printSentance(Rest).

%% writeWord, expects a word / atom to be printed
% writeWord write a word to stdout and formats special
% atoms to be formatted correct, for example that words have spaces
% between them and `qm` are printed as `?`
writeWord(qm)  :- write("?").
writeWord(i)  :- write("I"), write(" "). % makes output easier to read 
writeWord(Word):- write(Word), write(" ").

% takes in a question and resturns an answer
% 1st argument is the question
% 2nd argument is the answer
% This cannot go from an answer to a question, see report.pdf for more
answer([], []).
answer(Question, Answer):-
     match(Question, Out, Rest),
     ( Rest = [] -> Partial = [qm] ; answer(Rest, Partial)),
     append(Out, Partial, Answer).

% match a pharse
% 1st argument is the input array and matches against them
% 2nd argument is the translated sentance
% 3rd argument is the rest of the sentance to be matched
% This cannot go from the outputs to an input
match([], [], []).
match(Match, Out, Tail):-
    Match = [M | RestMatch],
    (
        M = i   -> matchI(RestMatch, Out, Tail) ;
        M = you -> ( Out = [your], Tail = RestMatch ) ;
        M = am  -> ( Out = [me], Tail = RestMatch ) ;
        M = my  -> ( Out = [your], Tail = RestMatch ) ;
        % does not match against anything so just return the word back and move forward a step
        (Out = [M], Tail = RestMatch) 
    ).

% match all the words that start with I
% 1st argument is the input array and matches against them
% 2nd argument is the translated sentance
% 3rd argument is the rest of the sentance to be matched
% This cannot go from the outputs to an input
matchI([], [you], []).
matchI(Match, Out, Tail):-
    Match = [M | Rest],
    (
        M = like ->       (Out = [why, you, like], Tail = Rest) ;
        M = know ->       (Out = [are, you, sure, you, know, that], Tail = Rest) ;
        M = feel ->       (Out = [what, makes, you, feel], Tail = Rest) ;
        M = am   ->       (Out = [you, are], Tail = Rest) ;
        % has the extra step of adding the "before" to the end of the sentance so it reads slightly better
        M = fantasised -> (Out = [have, you, ever, fantasised], append(Rest, [before], Tail)) ;
        % does not match against anything so just return the word back and move forward a step
        (Out = [you], Tail = Match) 
    ).

% printReply
printReply(Question):- answer(Question, Answer), printSentance(Answer).

%% Tests
% basic tests
:- answer([hello], [hello, qm]).
:- answer([], []).
:- answer([i], [you, qm]).

% questions and answers from the handout
:- answer([i, fantasised, about, fast, cars], [have, you, ever, fantasised, about, fast, cars, before, qm]).
:- answer([i, feel, bad, about, my, brother], [what, makes, you, feel, bad, about, your, brother, qm]). 
:- answer([i, know, i, am, insecure], [are, you, sure, you, know, that, you, are, insecure, qm]).
:- answer([i, like, my, mother], [why, you, like, your, mother, qm]).

% vim: set sw=4 ts=4 expandtab ft=prolog: 
