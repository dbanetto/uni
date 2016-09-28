% the known road network with distance
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

% road/3
% To   - 
% From - 
% make the roads symmetric
road(X, Y, D):- roaded(X, Y, D).
road(X, Y, D):- roaded(Y, X, D).

% route/3
% Finds a route from Start to finish
% If successful it will check if the route visits all the destinations
% returns True if there exists a route from start to finish and visits all destinations
% Start  - is the starting position of the journey
% Finish - is the destination of the journey
% Visits - is a list of destinations that need to visited on the route
route(Start, Finish, Visits):-
    find_route(Start, Finish, [], Route),
    visits_all(Route, Visits).

% route/4
% Finds a route from Start to finish
% If successful it will check if the route visits all the destinations
% If it does, it will calculate the distance taken on the route
% Start    - is the starting position of the journey
% Finish   - is the destination of the journey
% Visits   - is a list of destinations that need to visited on the route
% Distance - is the total distance the route takes
% returns True if there exists a route from start to finish and visits all destinations
route(Start, Finish, Visits, Distance):-
    find_route(Start, Finish, [], Route),
    visits_all(Route, Visits),
    sum_route(Route, Distance).

% choice/3
% Returns a list of tuples with routes taken and distance of the route
% Start              - is the starting position of the journey
% Finish             - is the destination of the journey
% RoutesAndDistances - is the output, which is a list of tuples in the form (Route, Distance)
choice(Start, Finish, RoutesAndDistances):-
    findall((R, D), (
        find_route(Start, Finish, [], R),
        sum_route(R, D)
    ), RoutesAndDistances).

% via/4
% Returns a list of tuples with routes taken and distance of the route
% Start              - is the starting position of the journey
% Finish             - is the destination of the journey
% Via                - is the list of destinations that the route must go through
% RoutesAndDistances - is the output, which is a list of tuples in the form (Route, Distance)
via(Start, Finish, Via, RoutesAndDistances):-
    findall((R, D), (
        find_route(Start, Finish, [], R),
        visits_all(R, Via),
        sum_route(R, D)
    ), RoutesAndDistances).


% avoiding/4
% Returns a list of tuples with routes taken and distance of the route
% Start              - is the starting position of the journey
% Finish             - is the destination of the journey
% Avoiding           - is the list of destinations that the route must not go through
% RoutesAndDistances - is the output, which is a list of tuples in the form (Route, Distance)
avoiding(Start, Finish, Avoiding, RoutesAndDistances):-
    findall((R, D), (
        find_route(Start, Finish, Avoiding, AR),
        remove_list(AR, Avoiding, R),
        sum_route(R, D)
    ), RoutesAndDistances).

% visits_all/2
% checks if the Route visits all of the destinations
% Route  - The route taken
% Visits - The list of destinations to check
visits_all(_, []). % base case of none left is true
visits_all(Route, Visits):-
    Visits = [Visit | Rest], % check a destination along the route
    member(Visit, Route),
    visits_all(Route, Rest). % look for one less destinations

% sum_route/2
% sum the route
% Route - the rest of the route to calculate in cost
% Base  - the amount calculated before
% Total - the total amount of distance
sum_route([_], 0).  % base case of none left the total is the base
% recursive case, removes one city from the route and puts back the other
% this is so you can do: a -> b -> c then b -> c.
sum_route(Route, Total):-
    Route = [From, To | Rest], % distance is only between two destinations
    sum_route([To | Rest], SubTotal), % get the subtotal for the rest of the list
    road(From, To, Distance),
    Total is SubTotal + Distance. % add distances

% remove_list/3
% utility func to remove a list of elements from an array
% Is NOT false if elements removed do not exist in the input array
% In     - the array that will get elements removed from
% Remove - the array of elements that should be removed
% Out    - the resulting array with removed elements
remove_list(Out, [], Out). % nothing to remove so pipe the input as output
remove_list(In, Remove, Out):-
    [Rm | Rest] = Remove,
    delete(In, Rm, Mod),
    remove_list(Mod, Rest, Out).


% find_route/4
% Start  - is the starting location of the search
% Finish - is the ending location of the search
% Taken  - is the path taken so far
% Route  - is the route from the 

% base case of Start = Finish, so the Route to the end should be the
% route taken so far + the end location. 
% The list is reversed as it is built out from the Finish point
find_route(Start, Finish, Taken, Route):- Start = Finish, [Finish | Taken] = Route.

% recursive case, steps the Start point forward 
find_route(Start, Finish, Taken, Route):-
    road(Start, Next, _),    % assuming non-directional graph.
    \+ member(Start, Taken), % make sure we have not been there before
    \+ member(Next, Taken),  % cut down on checks if we have been to Next before 
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

:- visits_all([auckland], [auckland]).
:- visits_all([wellington, new_plymouth, gisborne], [new_plymouth]).
:- \+ visits_all([wellington], [auckland]).

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
