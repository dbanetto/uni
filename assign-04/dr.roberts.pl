%% Questions and expected outputs
question([i, fantasised, about, fast, cars]).
% expected: [have, you, ever, fantasised, about, fast, cars, before, qm]
question([i, feel, bad, about, my, brother]).
% expected: [what, makes, you, feel, bad, about, your, brother, qm] 
question([i, know, i, am, insecure]). 
% expected:  [are, you, sure, you, know, that, you, are, insecure, qm]

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

answer([], []).
answer(Question, Answer):-
     match(Question, Out, Rest),
     answer(Rest, Partial),
     append(Out, Partial, Answer).

% match(Q, Out, Rest):-
%     [ i, W | Rest] = Q -> translate(W, Out) ; [ W | Rest ] = Q, Out = [W].

match([i, know | Tail], [are, you, sure, you, know, that], Tail).
match([i, feel | Tail], [what, makes, you, feel], Tail).
match([i, fantasised | Rest], [have, you, ever, fantasised], Tail):- append(Rest, [before], Tail).
match([not, working | Tail], [have, you tired, turning, it, off, and, on, again], Tail).
match([my | Tail], [your], Tail).
match([you | Tail], [me], Tail).
match([i | Tail], [you], Tail).
match([am | Tail], [me], Tail).
match([Word | Tail], [Word], Tail).

%
translate(know, [are, you, sure, you, know, that]).
translate(am, [you, are]).
translate(feel, [what, makes, you, feel]).

matches([], _).
matches(Keys, Rest):-
    [ K | Eys ] = Keys,
    [ R | Est ] = Rest,
    K == R -> matches(Eys, Est).
    

% printReply
printReply(Question):- answer(Question, Answer), printSentance(Answer).


% vim: set sw=4 ts=4 expandtab ft=prolog: 
