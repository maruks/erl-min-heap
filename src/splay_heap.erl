-module(splay_heap).

%% persistent splay heap

-export([empty/1,from_list/2,find_min/1,delete_min/1,insert/2,min_heap/1,max_heap/1,to_list/1,heap_size/1,is_empty/1]).

-record(node,{left :: undefined | #node{},
	      elem :: any(),
	      right :: undefined | #node{}}).

-record(heap,{comparator = fun(A,B) -> A=<B end,
	      size = 0 :: integer(),
	      root :: undefined | #node{}}).

%% types

-type elem() :: any().

-type cmp_fn() :: fun((elem(),elem())-> boolean()).

%% API

% returns empty heap with specified comparison function
-spec empty(cmp_fn()) -> #heap{size::0}.

empty(Comparator) ->
    #heap{comparator=Comparator}.

% creates heap from list
-spec from_list(cmp_fn(), [elem()]) -> #heap{}.

from_list(Comparator, Xs) ->
    lists:foldl(fun insert/2, empty(Comparator), Xs).

% returns all elements as list
-spec to_list(#heap{}) -> [elem()].

to_list(#heap{root=Root}) ->
    to_list_(Root).

% finds minimum element
-spec find_min(#heap{}) -> elem().

find_min(#heap{root=Root}) ->
    find_min_(Root).

% inserts element into heap
-spec insert(elem(),#heap{}) -> #heap{}.

insert(Element, #heap{comparator=Comparator, size=Size, root=Root} = Heap) ->
    Heap#heap{size = Size + 1, root = insert_(Element, Comparator, Root)}.

% returns heap size
-spec heap_size(#heap{}) -> integer().

heap_size(#heap{size=Size}) ->
    Size.

% returns true if heap is empty
-spec is_empty(#heap{}) -> boolean().

is_empty(#heap{size=Size}) ->
    Size =:= 0.

% deletes min element
-spec delete_min(#heap{}) -> #heap{}.

delete_min(#heap{size=Size, root=Root} = Heap) ->
    Heap#heap{size=Size - 1, root=delete_min_(Root)}.

% creates min heap
-spec min_heap([integer()]) -> #heap{}.

min_heap(Xs) ->
    from_list(fun(A,B) -> A=<B end,Xs).

% creates max heap
-spec max_heap([integer()]) -> #heap{}.

max_heap(Xs) ->
    from_list(fun(A,B) -> A>=B end,Xs).

%% Internals

insert_(Element, Comparator, Node) ->
    {LeftNode, RightNode} = partition(Comparator, Element, Node),
    #node{left=LeftNode, elem=Element, right=RightNode}.

find_min_(undefined) ->
    throw(empty_heap);
find_min_(#node{left=undefined, elem=Elem}) ->
    Elem;
find_min_(#node{left=Left}) ->
    find_min_(Left).

to_list_(#node{left=Left, elem=Elem, right=Right}) ->
    to_list_(Left) ++ [Elem] ++ to_list_(Right);
to_list_(undefined) ->
    [].

delete_min_(undefined) ->
    throw(empty_heap);
delete_min_(#node{left=undefined, right=Right}) ->
    Right;
delete_min_(#node{left=#node{left=undefined, right=ChildRight}, right=Right} = Node) ->
    Node#node{left=ChildRight, right=Right};
delete_min_(#node{left=#node{right=ChildRight} = LeftChild} = Node) ->
    delete_min_(LeftChild#node{right= Node#node{left=ChildRight}}).

partition2(_, #node{right=undefined} = Node, _, true) ->
    {Node, undefined};
partition2(Comparator, #node{right=#node{left=ChildLeft, elem=ChildElem, right=ChildRight} = RightChild} = Node, Pivot, true) ->
    LT = Comparator(ChildElem, Pivot),
    if LT ->
	    {Small, Big} = partition(Comparator, Pivot, ChildRight),
	    {#node{left=Node#node{right=ChildLeft}, elem=ChildElem, right=Small}, Big};
       true ->
	    {Small, Big} = partition(Comparator, Pivot, ChildLeft),
	    {Node#node{right=Small}, RightChild#node{left=Big}}
    end;
partition2(_ , #node{left=undefined} = Node, _, false) ->
    {undefined, Node};
partition2(Comparator, #node{left=#node{left=ChildLeft, elem=ChildElem, right=ChildRight} = LeftChild} = Node, Pivot, false) ->
    LT = Comparator(ChildElem, Pivot),
    if LT ->
	    {Small, Big} = partition(Comparator, Pivot, ChildRight),
	    {LeftChild#node{right=Small}, Node#node{left=Big}};
       true ->
	    {Small, Big} = partition(Comparator, Pivot, ChildLeft),
	    {Small, #node{left=Big, elem=ChildElem, right=Node#node{left=ChildRight}}}
    end.

partition(_, _, undefined) ->
    {undefined, undefined};
partition(Comparator, Pivot, #node{elem=Element} = Node) ->
    partition2(Comparator, Node, Pivot, Comparator(Element, Pivot)).
