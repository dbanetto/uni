
roaded(wellington,       palmerston_north, 143).
roaded(palmerston_north, wanganui,         74).
roaded(palmerston_north, napier,           178).
roaded(palmerston_north, taupo,            259).
roaded(wanganui,         taupo,            231).
roaded(wanganui,         new_plymouth,     163).
roaded(wanganui,         napier,           252).
roaded(napier,           taupo,            147).
roaded(napier,           gisborne,         215).
roaded(new_plymouth,     hamilton,         242).
roaded(new_plymouth,     taupo,            289).
roaded(taupo,            hamilton,         153).
roaded(taupo,            rotorua,           82).
roaded(taupo,            gisborne,         334).
roaded(gisborne,         rotorua,          291).
roaded(rotorua,          hamilton,         109).
roaded(hamilton,         auckland,         126).

road(X, Y, D):- roaded(X, Y, D).
road(X, Y, D):- roaded(Y, X, D).

% Finds a route from Start to finish
% If successful it will check if the route visits all the destinations
route(Start, Finish, Visits):-
    find_route(Start, Finish, [], Route),
    visits_all(Route, Visits).

% Finds a route from Start to finish
% If successful it will check if the route visits all the destinations
% If it does, it will calculate the distance taken on the route
route(Start, Finish, Visits, Distance):-
    find_route(Start, Finish, [], Route),
    visits_all(Route, Visits),
    sum_route(Route, Distance).

choice(Start, Finish, RoutesAndDistances):-
    findall((R, D), (
        find_route(Start, Finish, [], R),
        sum_route(R, D)
    ), RoutesAndDistances).

via(Start, Finish, Via, RoutesAndDistances):-
    findall((R, D), (
        find_route(Start, Finish, [], R),
        visits_all(R, Via),
        sum_route(R, D)
    ), RoutesAndDistances).


avoiding(Start, Finish, Avoiding, RoutesAndDistances):-
    findall((R, D), (
        find_route(Start, Finish, Avoiding, AR),
        remove_list(AR, Avoiding, R),
        sum_route(R, D)
    ), RoutesAndDistances).

% checks if the Route visits all of the destinations
% Route - The route taken
% Visits - The list of destinations to check
visits_all(_, []). % base case of none left is true
visits_all(Route, Visits):-
    Visits = [Visit | Rest], % check a destination
    member(Visit, Route),
    visits_all(Route, Rest). % look for one less destinations

% sum the route
% Route - the rest of the route to calculate in cost
% Base - the amount calculated before
% Total - the total amount of distance
sum_route([_], 0).  % base case of none left the total is the base
% recursive case, removes one city from the route and puts back the other
% this is so you can do: a -> b -> c then b -> c.
sum_route(Route, Total):-
    Route = [From, To | Rest],
    sum_route([To | Rest], SubTotal),
    road(From, To, Distance),
    Total is SubTotal + Distance. % add distances

remove_list(Out, [], Out).
remove_list(In, Remove, Out):-
    [Rm | Rest] = Remove,
    delete(In, Rm, Mod),
    remove_list(Mod, Rest, Out).


% find_route/4
% Start - is the starting location of the search
% Finish - is the ending location of the search
% Taken - is the path taken so far
% Route - is the route from the 

% base case of Start = Finish, so the Route to the end should be the
% route taken so far + the end location. 
% The list is reversed as it is built out from the Finish point
find_route(Start, Finish, Taken, Route):- Start = Finish, [Finish | Taken] = Route.

% recursive case, steps the Start point forward 
find_route(Start, Finish, Taken, Route):-
    road(Start, Next, _), % assuming non-directional graph.
    \+ member(Start, Taken), % make sure we have not been there before
    \+ member(Next, Taken), % make sure we have not been there before
    find_route(Next, Finish, [Start | Taken], Route). % take the next step to completion

%% tests
% test symmetric property of road
:- road(wellington, palmerston_north, _).
:- road(palmerston_north, wellington,  _).

:- route(wellington, auckland, []).
:- route(auckland, wellington, []).
:- route(hamilton, napier, []).
:- route(napier, hamilton, []).

:- sum_route([auckland], 0).
:- sum_route([auckland, hamilton], 126).
:- sum_route([auckland, hamilton, rotorua], 235).

:- route(wellington, auckland, [], 727).
:- route(auckland, wellington, [], 727).
:- route(hamilton, napier, [], 615).
:- route(napier, hamilton, [], 615).

:- choice(wellington, palmerston_north, RD), length(RD, 1). 
:- choice(palmerston_north, wellington, RD), length(RD, 1). 

:- choice(wellington, auckland, RD), length(RD, 52). 
:- choice(auckland, wellington, RD), length(RD, 52). 

:- choice(gisborne, rotorua, RD), length(RD, 35). 
:- choice(rotorua, gisborne, RD), length(RD, 35). 

:- via(wellington, palmerston_north, [], RD), length(RD, 1). 
:- via(wellington, palmerston_north, [wellington], RD), length(RD, 1). 
:- via(wellington, palmerston_north, [auckland], RD), length(RD, 0). 

:- via(wellington, auckland, [], RD), length(RD, 52). 
:- via(wellington, auckland, [rotorua], RD), length(RD, 29). 
:- via(wellington, auckland, [wanganui], RD), length(RD, 37). 

:- remove_list([], [], []).
:- remove_list([], [1], []).
:- remove_list([1], [1], []).
:- remove_list([1, 2], [1], [2]).

:- avoiding(wellington, auckland, [], RD), length(RD, 52).
:- avoiding(wellington, auckland, [hamilton], RD), length(RD, 0).
:- avoiding(wellington, auckland, [wanganui], RD), length(RD, 15).

% vim: set sw=4 ts=4 expandtab ft=prolog:
