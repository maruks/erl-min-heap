-module(splay_heap).

%% persistent splay heap

-export([empty/1,from_list/2,find_min/1,delete_min/1,insert/2,integer_min_heap/1]).

%% types

-type elem() :: any().

-type tree_node() :: empty
		   | {tree_node(), elem(), tree_node()}.

-type cmp_fn() :: fun((elem(),elem())-> atom()).

-type splay_heap() :: {cmp_fn(), tree_node()}.

-spec empty(cmp_fn()) -> splay_heap().
-spec from_list(cmp_fn(), [elem()]) -> splay_heap().
-spec find_min(splay_heap()) -> elem().
-spec insert(elem(),splay_heap()) -> splay_heap().
-spec delete_min(splay_heap()) -> splay_heap().
-spec integer_min_heap([integer()]) -> splay_heap().

%% API

% returns empty heap with specified comparison function
empty(CmpFn) ->
    {CmpFn, empty}.

% creates heap from list
from_list(CmpFn, Xs) ->
    lists:foldl(fun insert/2, empty(CmpFn), Xs).

% finds minimum element
find_min({_,Root}) ->
    find_min_(Root).

% inserts element into heap
insert(E, {CmpFn,Root}) ->
    {CmpFn, insert_(E, CmpFn, Root)}.

% deletes min element
delete_min({CmpFn,Root}) ->
    {CmpFn, delete_min_(Root)}.

% creates integer min heap
integer_min_heap(Xs) ->
    from_list(fun(A,B) -> A=<B end,Xs).

%% Internals

insert_(E, CmpFn, Node) ->
    {LeftNode, RightNode} = partition(CmpFn, E, Node),
    {LeftNode, E, RightNode}.

find_min_(empty) ->
    throw(empty_heap);
find_min_({empty, E, _R}) ->
    E;
find_min_({L , _E, _R}) ->
    find_min_(L).

delete_min_(empty) ->
    throw(empty_heap);
delete_min_({empty, _E, R}) ->
    R;
delete_min_({{empty, _, R1}, E, R2}) ->
    {R1, E, R2};
delete_min_({{L1, E1, R1}, E2, R2}) ->
    delete_min_({L1, E1, {R1, E2, R2}}).

partition2(_, N = {_L, _E, empty}, _, true) ->
    {N, empty};
partition2(CmpFn, {L, E, {L2, E2, R2}}, Pivot, true) ->
    if E2 =< Pivot ->
	    {Small, Big} = partition(CmpFn, Pivot, R2),
	    {{{L, E, L2}, E2, Small}, Big};
       true ->
	    {Small, Big} = partition(CmpFn, Pivot, L2),
	    {{L, E, Small},{Big, E2, R2}}
    end;
partition2(_ , N = {empty, _E, _R}, _, false) ->
    {empty, N};
partition2(CmpFn, {{L2, E2, R2}, E, R}, Pivot, false) ->
    if E2 =< Pivot ->
	    {Small, Big} = partition(CmpFn, Pivot, R2),
	    {{L2, E2, Small},{Big, E, R}};
       true ->
	    {Small, Big} = partition(CmpFn, Pivot, L2),
	    {Small, {Big, E2, {R2, E, R}}}
    end.

partition(_, _Pivot, empty) ->
    {empty, empty};
partition(CmpFn, Pivot, N = {_L, E, _R}) ->
    partition2(CmpFn, N, Pivot, E =< Pivot).
