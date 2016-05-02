%%
%% @doc test module quperl_octree2.
%%
%% @author Lutz Behnke <lutz.behnke@gmx.de>
%% @copyright 2015 Lutz Behnke
%%

%%
%% Tests TODO: new_volume/2
%%             common_prefix
%%             filter_full_area, box_to_volume, xval, yval, zval,
%%

-module(quperl_octree_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("quperl_octree.hrl").


%%
%% Fixtures
%%


api_test_() ->
    { "provide minimal api tests",
      setup,
      fun setup_env/0,
      fun takedown_env/1,
      fun(Args) -> [
                    ?_test(test_new_volume0(Args)),
                    ?_test(test_new_volume1(Args)),
                    ?_test(test_new_volume2(Args))
                   ]
      end }.

unit_test_() ->
    { "test each function directly",
      setup,
      fun setup_env/0,
      fun takedown_env/1,
      fun(_Args) -> [ ?_test(test_normalize_nodes())
                    , ?_test(test_normalize_points())
                    , ?_test(test_to_node_id())
                    , ?_test(test_to_node_list())
                    , ?_test(test_specific_points())
                    , ?_test(test_append_node())
                    , ?_test(test_first_node())
                    , ?_test(test_is_equal())
                    , ?_test(test_rest_nodes())
                    , ?_test(test_node_id_bit_count())
                    , ?_test(test_is_all_zeroes())
                    , ?_test(test_is_all_ones())
                    , ?_test(test_default_max_depth())
                    , ?_test(test_xor_dim())
                    , ?_test(test_split())
                    , ?_test(test_sweep())
                    , ?_test(test_inner())
                    , ?_test(test_common_prefix())
                    , ?_test(test_filter_full_area())
                    , ?_test(test_box_to_volume())
                    ]
      end }.

setup_env() -> {args}.

takedown_env(_Args) -> ok.

test_new_volume0(_Args) ->

    Volume = quperl_octree:new_volume(),

    ?assertEqual(?DEFAULT_MAX_DEPTH, Volume#ot_volume.max_depth),

    ok.

test_new_volume1(_Args) ->

    Volume = quperl_octree:new_volume(10),

?assertEqual(10, Volume#ot_volume.max_depth),

ok.

test_new_volume2(_Args) ->

    TestCases = [{[],{0.479, 0.499, 0.499}, {0.501, 0.501, 0.501}},
                 {[],{0.479, 0.479, 0.499}, {0.501, 0.501, 0.501}},
                 {[],{0.479, 0.499, 0.479}, {0.501, 0.501, 0.501}},
                 {[],{0.499, 0.499, 0.499}, {0.501, 0.501, 0.501}},
                 {{badarg, val}, {0.0, 0.0, 0.0}, {2.0, 2.0, 2.0}}],

    lists:foreach(fun({R,P1, P2}) ->
                          N1 =quperl_octree:to_node_id(P1),
                          N2 =quperl_octree:to_node_id(P2),
                          L1 = quperl_octree:box_to_volume([{N1,N2}],[]),
                          ?debugFmt("~nP1: ~p~nP2: ~p~nbox: ~p",
                                    [quperl_octree:to_node_list(N1),
                                     quperl_octree:to_node_list(N2),
                                     L1]),
                          ?assertEqual(R, catch quperl_octree:new_volume(P1, P2))
                          end, TestCases),

    ok.


test_inner() ->

%%     ?debugFmt("~n     [1,2,3]: ~p~ninner([1,2,3,4]): ~p",[new_node_id([1,2,3]),
%%                                                           inner(new_node_id([1,2,3,4]))]),

    ?assert(is_equal(new_node_id([1,2,3]),inner(new_node_id([1,2,3,4])))),
    ?assertThrow(zero_depth, inner(new_node_id([]))),

    ok.

test_box_to_volume() ->

    TestVals = [ {[], [quperl_octree:to_node_id([1])], [[1]]}
               , {[{quperl_octree:to_node_id([1,0]),
                    quperl_octree:to_node_id([1,7])}], [],
                  [[1]]}
               ],
    lists:foreach(fun({L,R, Out}) ->
        ?assertEqual(Out, as_node_list(quperl_octree:box_to_volume(L, R)))
                          end, TestVals),

    ok.


test_filter_full_area() ->

    Inputs = [{[],
               [],
               []},
              {[{[3,1,6],[3,1,6]}],
               [],
               [[3,1,6]]},
              {[{[3,1,0],[3,1,7]}],
               [],
               [[3,1]]},
              {[{[3,1,1],[3,1,7]}],
               [{[3,1,1],[3,1,7]}],
               []},
              {[{[3,1,1],[3,1,7]},{[3,1,0],[3,1,7]}],
               [{[3,1,1],[3,1,7]}],
               [[3,1]]},
              {[{[3,4,5,0],[3,4,5,0]},  % this function does not sweep, but must maintain sort order
                {[3,4,5,1],[3,4,5,1]},
                {[3,4,5,2],[3,4,5,2]},
                {[3,4,5,3],[3,4,5,3]},
                {[3,4,5,4],[3,4,5,4]},
                {[3,4,5,5],[3,4,5,5]},
                {[3,4,5,6],[3,4,5,6]},
                {[3,4,5,7],[3,4,5,7]}],
               [],
               [[3,4,5,0],
                [3,4,5,1],
                [3,4,5,2],
                [3,4,5,3],
                [3,4,5,4],
                [3,4,5,5],
                [3,4,5,6],
                [3,4,5,7]]}
             ],

    lists:foreach(fun({In, Split, Areas}) ->

                          InNodes = lists:map(fun({P1, P2}) -> {quperl_octree:to_node_id(P1),
                                                                quperl_octree:to_node_id(P2)} end,
                                              In),

                          {SplitResult, AreaResult} = quperl_octree:filter_full_area(InNodes),
                          ?assertEqual({Split,Areas},
                                       {as_node_list(SplitResult),
                                        as_node_list(AreaResult)})
                          end, Inputs),

    ok.


test_sweep() ->

    Inputs = [{[],[]},
              {[[1,0]],
               [[1,0]]},
              {[[1,0],[1,1]],
               [[1,0],[1,1]]},
              {[[1,0],[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[1,7]],
               [[1]]},
              {[[1,0],[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[1,7],[2,0]],
               [[1],[2,0]]},
              {[[1,0,1],[1,1,1],[1,2,1],[1,3,1],[1,4,1],[1,5,1],[1,6,1],[1,7,1]],
               [[1,0,1],[1,1,1],[1,2,1],[1,3,1],[1,4,1],[1,5,1],[1,6,1],[1,7,1]]},
              {[[1,0,0],[1,1,0],[1,2,0],[1,3,0],[1,4,0],[1,5,0],[1,6,0],[1,7,0]],
               [[1,0,0],[1,1,0],[1,2,0],[1,3,0],[1,4,0],[1,5,0],[1,6,0],[1,7,0]]},
              {[[1,0,0],[1,1,1],[1,2,2],[1,3,3],[1,4,4],[1,5,5],[1,6,6],[1,7,7]],
               [[1,0,0],[1,1,1],[1,2,2],[1,3,3],[1,4,4],[1,5,5],[1,6,6],[1,7,7]]},
              {[[1,0,0],[1,1,1],[1,2,2],[1,3,3],[1,3,4],[1,5,5],[1,6,6],[1,7,7]],
               [[1,0,0],[1,1,1],[1,2,2],[1,3,3],[1,3,4],[1,5,5],[1,6,6],[1,7,7]]}
             ],

    lists:foreach(fun({In, Out}) ->
                          ?assertEqual(Out, quperl_octree:sweep(In))
                          end, Inputs),

    ok.
test_common_prefix() ->

        Inputs = [ {[0],[0],[0],[],[]}
                 , {[3,1,6],[3,1,2], [3,1],[6],[2]}
                 ],

    lists:foreach(fun({Point1,Point2,Pre,Post1,Post2}) ->

                          ?assertEqual({quperl_octree:to_node_id(Pre),
                                        quperl_octree:to_node_id(Post1),
                                        quperl_octree:to_node_id(Post2)},
                                       quperl_octree:common_prefix(quperl_octree:to_node_id(Point1), quperl_octree:to_node_id(Point2)))

                          end, Inputs),

        Inputs2 = [{{0.585702840753962, 0.8471973103830974, 0.7459485676749666},
                    {1.0e-6,1.0e-6,1.0e-6},
                    [7,2,1,7,3,5,1,4,7,7,7,5,0,1,1,2]}, % depth = 16

                   {{0.000001, 0.000001, 0.000001},
                    {0.9999, 0.9999, 0.9999},
                    []},

                   {{0.1, 0.1, 0.1},
                    {0.16, 0.16, 0.16},
                    [0]},

                   {{0.5, 0.5, 0.5},
                    {0.26, 0.26, 0.26},
                    [7]},

                   {{0.1, 0.1, 0.1},
                    {0.01, 0.01, 0.01},
                    [0,0,0,7,7]},

                   {{0.1, 0.2, 0.3},
                    {0.01, 0.01, 0.01},
                    [0,1,2,6,5]},

                   {{0.01, 0.01, 0.01},
                    {0.5, 0.5, 0.5},
                    []},

                   {{1.0e-7, 1.0e-7, 1.0e-7},
                    {1.0e-6, 1.0e-6, 1.0e-6},
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]},  % depth = 19

                   {{0.01, 0.01, 0.01},
                    {0.1, 0.2, 0.3},
                    [0]},

                   {{0.01, 0.01, 0.01},
                    {0.0, 0.0, 0.0},
                    [0,0,0,0,0,0,7,0,7,0,
                     0,0,7,7,7,7,0,7,0,7,
                     7,7,0,0,0,0,7,0,7,0,
                     0,0,7,7,7,7,0,7,0,7,
                     7,7,0,0,0,0,7,0,7,0,
                     0,0,7,7,7,7,0,7,7,0]}   % depth = DEFAULT_MAX_DEPTH
                  ],

        lists:foreach(fun({Position = {P1, P2, P3}, {B1, B2, B3}, ExpectedAC}) ->
                              {Pre, _R1, _R2} =
                                  quperl_octree:common_prefix(quperl_octree:to_node_id(Position),
                                                              quperl_octree:to_node_id({P1 + B1, P2 + B2, P3 + B3})),
                              ?assertEqual(ExpectedAC, quperl_octree:to_node_list(Pre))

                      end, Inputs2),

    ok.


%% // does not have test for length(TreePrefix) == MaxDepth, as this would mean
%% // that P1 and P2 are equal
%% split(TreePrefix, RestDepth,P1, P2):
%%   F1 = first(P1)
%%   F2 = first(P2)
%%
%%   if F1 == F2
%%     R1 = rest(P1)
%%     R2 = rest(R2)
%%     return split([F1 | TreePrefx], RestDepth-1, R1, R2)
%%   else
%%     split_list = split_borders([{P1,P2}],x)
%%     split_list = split_borders(split_list,y)
%%     split_list = split_borders(split_list,z)
%%     return prepend_each(split_list, TreePrefix)  # returns 2-8 areas
%%
%% This function will not convert tuples of (P1,P2) with P1 == P2 into
%% node ids, this should be done by the calling function.
%%
test_split() ->
    TestVals = [ %% {[0],[0]} excluded as it would be P1 == P2, excluded by precondition
                {[0],[1],   [{[0],[0]}, {[1],[1]} ]},
                {[0],[2],   [{[0],[0]}, {[2],[2]} ]},
                {[0],[3],
                 [{[0],[0]}, {[1],[1]}, {[2],[2]}, {[3],[3]} ]},
                {[0],[4],   [{[0],[0]}, {[4],[4]} ]},
                {[0],[5],
                 [{[0],[0]}, {[1],[1]}, {[4],[4]}, {[5],[5]} ]},
                {[0],[6],
                 [{[0],[0]}, {[2],[2]}, {[4],[4]}, {[6],[6]} ]},
                {[0],[7],
                 [{[0],[0]}, {[1],[1]}, {[2],[2]}, {[3],[3]},
                  {[4],[4]}, {[5],[5]}, {[6],[6]}, {[7],[7]} ]},
                {[2,0],[2,1],
                 [
                  {[2,0],[2,0]},
                  {[2,1],[2,1]}
                 ]},
                {[3,2,0],[3,2,1],
                 [
                  {[3,2,0],[3,2,0]},
                  {[3,2,1],[3,2,1]}
                 ]},
                {[5,3,2,0],[5,3,2,1],
                 [
                  {[5,3,2,0],[5,3,2,0]},
                  {[5,3,2,1],[5,3,2,1]}
                 ]},
                {[1,2,3,4], [7,6,5,4],
                 [
                  {[1,2,3,4],[1,6,7,6]},
                  {[3,0,1,4],[3,6,5,4]},
                  {[5,2,3,0],[5,6,7,6]},
                  {[7,0,1,0],[7,6,5,4]}
                 ]}],


    lists:foreach(fun do_test_split/1, TestVals).

do_test_split({L1, L2, Expected}) ->
    P1 = quperl_octree:to_node_id(L1, ?DEFAULT_MAX_DEPTH),
    P2 = quperl_octree:to_node_id(L2, ?DEFAULT_MAX_DEPTH),

    Result = quperl_octree:split([], ?DEFAULT_MAX_DEPTH, P1, P2),

%%     ?debugFmt("~nInput:~n L1: ~p,~n L2: ~p,~n Result: ~p~n", [L1,L2, as_node_list(Result)]),
    timer:sleep(100),

    ?assertEqual(Expected,as_node_list(Result)),

    ok.


test_is_all_zeroes() ->
    ?assertEqual(true, quperl_octree:is_all_zeroes(quperl_octree:to_node_id([], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(true, quperl_octree:is_all_zeroes(quperl_octree:to_node_id([0], ?DEFAULT_MAX_DEPTH))),

    ?assertEqual(false, quperl_octree:is_all_zeroes(quperl_octree:to_node_id([4], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, quperl_octree:is_all_zeroes(quperl_octree:to_node_id([2], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, quperl_octree:is_all_zeroes(quperl_octree:to_node_id([1], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, quperl_octree:is_all_zeroes(quperl_octree:to_node_id([7], ?DEFAULT_MAX_DEPTH))),

    ?assertError(function_clause, quperl_octree:is_all_zeroes(0)),
    ok.

test_is_all_ones() ->
    ?assertEqual(false, quperl_octree:is_all_ones(quperl_octree:to_node_id([0], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, quperl_octree:is_all_ones(quperl_octree:to_node_id([4], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, quperl_octree:is_all_ones(quperl_octree:to_node_id([2], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, quperl_octree:is_all_ones(quperl_octree:to_node_id([1], ?DEFAULT_MAX_DEPTH))),

    %% matter of definition: the required number of 0's for a path of 0 is 0!
    ?assertEqual(true, quperl_octree:is_all_ones(quperl_octree:to_node_id([], ?DEFAULT_MAX_DEPTH))),

    ?assertEqual(false, quperl_octree:is_all_ones(quperl_octree:to_node_id([0,7], ?DEFAULT_MAX_DEPTH))),

    ?assertEqual(true, quperl_octree:is_all_ones(quperl_octree:to_node_id([7], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(true, quperl_octree:is_all_ones(quperl_octree:to_node_id([7,7], ?DEFAULT_MAX_DEPTH))),

    ?assertError(function_clause, quperl_octree:is_all_ones(3)),
    ok.

test_node_id_bit_count() ->
    ?assertEqual(1, quperl_octree:bit_count(#ot_node_id{depth=1, x=1, y =0, z=0})),
    ?assertEqual(1, quperl_octree:bit_count(#ot_node_id{depth=1, x=0, y =1, z=0})),
    ?assertEqual(1, quperl_octree:bit_count(#ot_node_id{depth=1, x=0, y =0, z=1})),
    ?assertEqual(3, quperl_octree:bit_count(#ot_node_id{depth=1, x=1, y =1, z=1})),

    Val = (1 bsl 63) + 1,
    ?assertEqual(6, quperl_octree:bit_count(#ot_node_id{depth=64, x=Val, y=Val, z=Val})),

    ok.

test_first_node() ->

    TestCases = [ {[1],1}
                , {[2],2}
                , {[4],4}
                , {[7],7}
                , {[0,1],0}
                ],

    lists:foreach(fun({List, Result}) ->
                          Node = quperl_octree:to_node_id(List, ?DEFAULT_MAX_DEPTH),
                          ?assertEqual(Result, quperl_octree:first_node(Node))
                          end, TestCases),

    ?assertThrow(zero_depth, quperl_octree:first_node(#ot_node_id{depth=0})),

    ok.

test_is_equal() ->
    NI1 = quperl_octree:to_node_id([], ?DEFAULT_MAX_DEPTH),
    ?assert(quperl_octree:is_equal(NI1, NI1)),

    NI2 = quperl_octree:to_node_id([1], ?DEFAULT_MAX_DEPTH),
    ?assert(quperl_octree:is_equal(NI2, NI2)),

    NI3 = quperl_octree:to_node_id([1,2,3,4,5], ?DEFAULT_MAX_DEPTH),
    ?assert(quperl_octree:is_equal(NI3, NI3)),

    ?assertNot(quperl_octree:is_equal(NI1, NI2)),
    ?assertNot(quperl_octree:is_equal(NI2, NI1)),

    ?assertNot(quperl_octree:is_equal(NI1, NI3)),
    ?assertNot(quperl_octree:is_equal(NI3, NI1)),

    NI4 = quperl_octree:to_node_id([4], ?DEFAULT_MAX_DEPTH),
    ?assert(quperl_octree:is_equal(NI4, NI4)),

    ?assertNot(quperl_octree:is_equal(NI2, NI4)),
    ?assertNot(quperl_octree:is_equal(NI4, NI2)),

    ok.

test_rest_nodes() ->

%%     ?debugFmt("~ndefault Depth:~n[2,3,4]: ~p~n[1,2,3,4]: ~p~nrest([1,2,3,4]): ~p",
%%               [to_node_id([2,3,4], ?DEFAULT_MAX_DEPTH),
%%                to_node_id([1,2,3,4], ?DEFAULT_MAX_DEPTH),
%%                rest_nodes(to_node_id([1,2,3,4], ?DEFAULT_MAX_DEPTH))]),

%%     ?debugFmt("~nDepth 64:~n[2,3,4]: ~p~n[1,2,3,4]: ~p~nrest([1,2,3,4]): ~p",
%%               [to_node_id([2,3,4], 64),
%%                to_node_id([1,2,3,4], 64),
%%                rest_nodes(to_node_id([1,2,3,4], 64))]),

    ?assert(quperl_octree:is_equal(quperl_octree:to_node_id([2,3,4], ?DEFAULT_MAX_DEPTH),
                     quperl_octree:rest_nodes(quperl_octree:to_node_id([1,2,3,4], ?DEFAULT_MAX_DEPTH)))),

    ?assertThrow(zero_depth, quperl_octree:rest_nodes(quperl_octree:to_node_id([], ?DEFAULT_MAX_DEPTH))),

    ok.


test_append_node() ->

    TestCases = [
                 {[2,3,1], [2,3], 1},
                 {[2,3,0], [2,3], 0},
                 {[0,3,1], [0,3], 1},
                 {[0,0,1], [0,0], 1},
                 {[1]    , []   , 1}
                ],

    lists:foreach(fun({Result, List, New}) ->
                          ?assertEqual(quperl_octree:to_node_id(Result, ?DEFAULT_MAX_DEPTH),
                                       quperl_octree:append_node(quperl_octree:to_node_id(List,?DEFAULT_MAX_DEPTH),
                                                   New)),
                          ok
                  end, TestCases),

    ok.

test_specific_points() ->
    Tests = find_point_testcases(),

    lists:foreach(fun({Input, Expected}) ->
                          Result = quperl_octree:to_node_list(quperl_octree:to_node_id(Input,?DEFAULT_MAX_DEPTH)),
%%                        ?debugFmt("testing: ~p",[Input]),
%%                        ?debugFmt("expected: ~P~n",[Expected,100]),
%%                        ?debugFmt("result:   ~P~n",[Result,100]),
                          match_significant(Expected, Result, 50)
                          end,
                  Tests),
    ok.


%% Test cases for finding individual points
%% TODO: {0.1e-15,0.1e-15,0.1e-15}
find_point_testcases() ->
    [{{0.0,0.0,0.0}, lists:duplicate(64, 0)},
     {{0.5,0.5,0.5}, [7] ++ lists:duplicate(63, 0)},
%%                            0                   1
%%                            0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
     {{0.51,0.51,0.51},      [7,0,0,0,0,0,7,0,7,0,0,0,7,7,7,7,0,7,0,7,
                              7,7,0,0,0,0,7,0,7,0,0,0,7,7,7,7,0,7,0,7,
                              7,7,0,0,0,0,7,0,7,0,0,7,0,0,0,0,0,0,0,0,
                              0,0,0,0]},
     {{1.0e-7,1.0e-7,1.0e-7},[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                              0,0,0,7,7,0,7,0,7,7,0,7,0,7,7,7,7,7,7,7,
                              0,0,7,0,7,0,0,7,7,0,7,0,7,0,7,7,7,7,0,0,
                              7,0,7,0]},
     {{0.0 , 0.0, 0.5}, [1] ++ lists:duplicate(63, 0)},
     {{0.0 , 0.5, 0.0}, [2] ++ lists:duplicate(63, 0)},
     {{0.0 , 0.5, 0.5}, [3] ++ lists:duplicate(63, 0)},
     {{0.0 , 0.25, 0.5}, [1,2] ++ lists:duplicate(62, 0)},
     {{0.0 , 0.25, 0.5}, [1,2] ++ lists:duplicate(62, 0)},
     {{0.0 , 0.375, 0.625}, [1,2,3] ++ lists:duplicate(61, 0)}
    ].

match_significant(Expected, Result, Significant) ->
    {ExSig, _} = lists:split(Significant,Expected),
    {ResSig, _} = lists:split(Significant,Result),
    ?assertEqual(ExSig,ResSig).

test_to_node_list() ->

    TestVals = [[0], [1], [2], [3], [7], [1,2], [1,2,3,4,5,6,7]],

    lists:foreach(fun(Val) ->
            ?assertEqual(Val,
                         quperl_octree:to_node_list(quperl_octree:to_node_id(Val,?DEFAULT_MAX_DEPTH)))
                  end, TestVals),

    ok.


test_normalize_nodes() ->
    ?assertThrow(different_point_depth_not_supported,
                 quperl_octree:normalize(#ot_node_id{depth=1}, #ot_node_id{depth=2})),

    P1 = #ot_node_id{depth=62,x=3,y=2,z=5},
    P2 = #ot_node_id{depth=62,x=1,y=5,z=2},
    P1s = #ot_node_id{depth=62,x=1,y=2,z=2},
    P2s = #ot_node_id{depth=62,x=3,y=5,z=5},
    ?assertEqual({P1s, P2s}, quperl_octree:normalize(P1, P2)),

    ?assertThrow(unsupported_point_type_combination,
                 quperl_octree:normalize(P1, {0.2, 0.3, 0.4})),

    ok.

test_normalize_points() ->

    TestCases = [{{{0.1, 0.2, 0.3},{0.4, 0.5, 0.6}}, {0.1,0.2,0.3}, {0.4,0.5,0.6}},

                 {{{0.1, 0.1, 0.1},{0.2, 0.1, 0.1}}, {0.1, 0.1, 0.1}, {0.2, 0.1, 0.1}},
                 {{{0.1, 0.1, 0.1},{0.1, 0.2, 0.1}}, {0.1, 0.1, 0.1}, {0.1, 0.2, 0.1}},
                 {{{0.1, 0.1, 0.1},{0.1, 0.1, 0.2}}, {0.1, 0.1, 0.1}, {0.1, 0.1, 0.2}},

                 {{{0.1, 0.1, 0.1},{0.2, 0.1, 0.1}}, {0.2, 0.1, 0.1}, {0.1, 0.1, 0.1}},
                 {{{0.1, 0.1, 0.1},{0.1, 0.2, 0.1}}, {0.1, 0.2, 0.1}, {0.1, 0.1, 0.1}},
                 {{{0.1, 0.1, 0.1},{0.1, 0.1, 0.2}}, {0.1, 0.1, 0.2}, {0.1, 0.1, 0.1}},

                 {{{0.1, 0.3, 0.5},{0.2, 0.4, 0.6}}, {0.1, 0.4, 0.6}, {0.2, 0.3, 0.5}}
                 ],

    lists:foreach(fun({Res, A, B}) ->
                          ?assertEqual(Res,
                                       quperl_octree:normalize(A,B))
                  end, TestCases),

    ok.


test_to_node_id() ->

    ?assertThrow({badarg, _Val}, quperl_octree:to_node_id({0.0, 0.0, 1.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, quperl_octree:to_node_id({0.0, 1.0, 0.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, quperl_octree:to_node_id({1.0, 0.0, 0.0}, ?DEFAULT_MAX_DEPTH)),

    ?assertThrow({badarg, _Val}, quperl_octree:to_node_id({0.0, 0.0, -1.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, quperl_octree:to_node_id({0.0, -1.0, 0.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, quperl_octree:to_node_id({-1.0, 0.0, 0.0}, ?DEFAULT_MAX_DEPTH)),

    ?assertMatch([7,0,0|_], quperl_octree:to_node_list(quperl_octree:to_node_id({0.5, 0.5, 0.5},
                                                    ?DEFAULT_MAX_DEPTH))),

    ok.


test_default_max_depth() ->
    ?assertEqual(?DEFAULT_MAX_DEPTH, quperl_octree:default_max_depth()),

    ok.




test_xor_dim() ->
    TestVals = [{0, 1, 1, z},
                {1, 0, 1, z},
                {1, 1, 0, z},
                {0, 0, 0, z},

                {0, 0, 2, z},
                {0, 0, 4, z},
                {0, 2, 2, z},
                {0, 4, 4, z},

                {0, 2, 2, y},
                {1, 0, 2, y},
                {1, 2, 0, y},
                {0, 0, 0, y},

                {0, 4, 4, x},
                {1, 0, 4, x},
                {1, 4, 0, x},
                {0, 0, 0, x},

                {1, 6, 7, z},
                {1, 5, 7, y},
                {1, 3, 7, x}],

    lists:foreach(fun({Expect, P1, P2, Dim}) ->
%%                           ?debugFmt("Expect : xor_dim(~p, ~p, ~p) -> ~p",[P1,P2,Dim,Expect]),
                          ?assertEqual(Expect, quperl_octree:xor_dim(P1,P2,Dim))
                          end, TestVals),
    ok.


%% @doc convert lists of lists of node_ids into node code lists
%% @private
-spec as_node_list(#ot_node_id{} | [#ot_node_id{}] | [{#ot_node_id{}, #ot_node_id{}}]) ->
          [ot_child_code()] | [ [ot_child_code()] ] | [{[ot_child_code()], [ot_child_code()]}].

as_node_list([]) -> [];

as_node_list([First|Rest]) when is_list(First) ->
    [ as_node_list(First) | as_node_list(Rest) ];

as_node_list([P1|Rest]) when is_record(P1, ot_node_id) ->
   [ quperl_octree:to_node_list(P1) | as_node_list(Rest)];

as_node_list([{P1, P2}|Rest]) ->
   [{quperl_octree:to_node_list(P1), quperl_octree:to_node_list(P2)} | as_node_list(Rest)].

