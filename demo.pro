:- module(evaluate, [main/0, my_predicate/2]).
:- use_module(library(prologbeans)).
:- use_module(library(codesio), [read_from_codes/2]).

eighteen_till_twentythree(Clock):- Clock >= 1800, Clock =< 2259.
twentythree_till_seven(Clock):- (Clock >= 2300, Clock =< 2359);
                                (Clock >= 0, Clock =< 659).
seven_till_eighteen(Clock):- Clock >= 700, Clock =< 1759.

lte_five(Sensor):- Sensor =< 5.
gt_five(Sensor):- Sensor > 5.

light(Clock, Sensor):- eighteen_till_twentythree(Clock);
                       (seven_till_eighteen(Clock), lte_five(Sensor)).
curtain(Clock, Sensor):- eighteen_till_twentythree(Clock);
                         twentythree_till_seven(Clock);
                         lte_five(Sensor).

get_time(Clock):- Clock is 2315.
get_sensor(Sensor):- Sensor is 10.

good_to_sleep:- get_time(Clock), get_sensor(Sensor), \+ light(Clock, Sensor), curtain(Clock, Sensor).

%% Register acceptable queries and start the server (using default port)
main:-
    register_query(evaluate(C, P), my_predicate(C, P)),
    start.

%% We have received a code-list
%% which needs to be converted into an expression
my_predicate(Chars, P) :-
    read_from_codes(Chars, X),
    X,
    P = yes.
