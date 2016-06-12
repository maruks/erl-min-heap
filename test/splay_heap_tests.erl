-module(splay_heap_tests).
-import(splay_heap,[empty/1,from_list/2,find_min/1,delete_min/1,insert/2,min_heap/1,max_heap/1,to_list/1]).

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

prop_min_elem() ->
    ?FORALL(Xs, non_empty(list(int())), find_min(min_heap(Xs)) == lists:min(Xs)).

prop_heap_sort() ->
    ?FORALL(Xs, list(int()), heap_sort(min_heap(Xs)) == lists:sort(Xs)).

prop_max_elem() ->
    ?FORALL(Xs, non_empty(list(int())), find_min(max_heap(Xs)) == lists:max(Xs)).

find_min_should_return_the_minimum_tuple_from_the_heap_test() ->
    Xs = [{foo, 3}, {bar, 2}, {lol, 4}],
    CmpFn = fun(A,B) -> element(2, A) < element(2, B) end,
    H = from_list(CmpFn, Xs),
    ?assertEqual(find_min(H), {bar,2}).

delete_min_from_empty_heap_should_throw_empty_heap_error_test() ->
    H = empty(fun(A,B) -> A<B end),
    ?assertThrow(empty_heap,delete_min(H)).

delete_min_should_delete_min_element_test() ->
    H = min_heap([1,2,3,4]),
    ?assertEqual(2, find_min(delete_min(H))).

delete_min_should_only_delete_one_element_test() ->
    H = min_heap([1,1]),
    Hexp = min_heap([1]),
    ?assertEqual(Hexp, delete_min(H)).

insert_should_add_an_element_test() ->
    H = min_heap([1,2]),
    H2 = min_heap([1,2,3]),
    ?assertEqual(H2, insert(3,H)).

insert_should_add_element_even_if_it_exists_test() ->
    H = min_heap([1]),
    Hexp = min_heap([1,1]),
    ?assertEqual(Hexp, insert(1,H)).

insert_should_add_a_heap_as_an_element_to_another_heap_test() ->
    H = min_heap([1]),
    Hnew = min_heap([2]),
    Hexp = min_heap([2,H]),
    ?assertEqual(Hexp, insert(H,Hnew)).

to_list_should_return_empty_list_on_empty_heap_test() ->
    H = min_heap([]),
    ?assertEqual([], to_list(H)).

to_list_should_return_all_elements_of_heap_test() ->
    H = min_heap([1,2,3,4]),
    ?assertEqual([1,2,3,4], to_list(H)).

heap_sort_should_return_a_sorted_list_with_the_elements_of_the_heap_test() ->
    Ilist = [3,2,1],
    H = min_heap(Ilist),
    Res = heap_sort(H),
    ?assertEqual([1,2,3], Res).

to_list_should_return_a_sorted_list_with_the_elements_of_the_heap_test() ->
    Ilist = [3,2,1,3,4,9],
    H = min_heap(Ilist),
    Res = to_list(H),
    ?assertEqual(lists:sort(Ilist), Res).
