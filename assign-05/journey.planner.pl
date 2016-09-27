
road(wellington,       palmerston_north, 143).
road(palmerston_north, wanganui,         74).
road(palmerston_north, napier,           178).
road(palmerston_north, taupo,            259).
road(wanganui,         taupo,            231).
road(wanganui,         palmerston_north, 163).
road(wanganui,         napier,           252).
road(napier,           taupo,            147).
road(napier,           gisborne,         215).
road(new_plymouth,     hamilton,         242).
road(new_plymouth,     taupo,            289).
road(taupo,            hamilton,         153).
road(taupo,            rotorua,          82).
road(taupo,            gisborne,         334).
road(gisborne,         rotorua,          291).
road(rotorua,          hamilton,         109).
road(hamilton,         auckland,         126).


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
    sum_route(Route, 0, Distance).

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
sum_route([_], Base, Total):- Base = Total. % base case of none left the total is the base
% recursive case, removes one city from the route and puts back the other
% this is so you can do: a -> b -> c then b -> c.
sum_route(Route, Base, Total):-
    Route = [From, To | Rest],
    ( road(From, To, Distance) ; road(To, From, Distance) ), % assuming non-directional graph
    NewBase is Base + Distance, % add distances
    sum_route([To | Rest], NewBase, Total).


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
    (road(Start, Next, _) ; road(Next, Start, _)), % assuming non-directional graph.
    not(member(Start, Taken)), % make sure we have not been there before
    find_route(Next, Finish, [Start | Taken], Route). % take the next step to completion


% vim: set sw=4 ts=4 expandtab ft=prolog:
