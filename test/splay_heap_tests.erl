-module(splay_heap_tests).
-import(splay_heap,[empty/1,from_list/2,find_min/1,delete_min/1,insert/2,integer_min_heap/1]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

heap_sort(H) ->
    lists:reverse(heap_sort([],H)).

heap_sort(Acc, {_,empty}) ->
    Acc;
heap_sort(Acc, H = {_,_}) ->
    E = find_min(H),
    heap_sort([E|Acc],delete_min(H)).

heap_sort_prop() ->
    ?FORALL(Xs, list(int()), heap_sort(integer_min_heap(Xs)) == lists:sort(Xs)).

min_elem_prop() ->
    ?FORALL(Xs, non_empty(list(int())), find_min(integer_min_heap(Xs)) == lists:min(Xs)).

heap_sort_test_() ->
    {"heap_sort_test", {timeout, 10, ?_assert(eqc:quickcheck(?MODULE:heap_sort_prop()))}}.

min_elem_test_() ->
    {"min_elem_test", {timeout, 10, ?_assert(eqc:quickcheck(?MODULE:min_elem_prop()))}}.
