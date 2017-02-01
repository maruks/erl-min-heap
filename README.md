# erl-min-heap
Persistent heaps from "Purely Functional Data Structures"

- [Splay heap](src/splay_heap.erl)

Build
-----

    $ rebar3 compile
    $ rebar3 eunit
    $ rebar3 eqc
    $ dialyzer _build/default/lib/min_heap/ebin
