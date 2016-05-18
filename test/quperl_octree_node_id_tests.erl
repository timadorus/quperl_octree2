%%
%% @doc test module quperl_octree2.
%%
%% @author Lutz Behnke <lutz.behnke@gmx.de>
%% @copyright 2015 Lutz Behnke
%%

%%
%% Tests TODO: common_prefix
%%             xval, yval, zval,
%%

-module(quperl_octree_node_id_tests).

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
      fun(Args) -> [ ?_test(test_is_node_id(Args))
                   , ?_test(test_children(Args))
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
                    , ?_test(test_default_max_depth())
                    , ?_test(test_inner())
                    , ?_test(test_leaf())
                    , ?_test(test_previous())
                    , ?_test(test_common_prefix())
                    , ?_test(test_for_each_child())
                    , ?_test(test_is_ancestor_of())
                    , ?_test(test_beyond())
                    , ?_test(test_get_depth())
                    , ?_test(test_bitlist())
                    ]
      end }.

setup_env() -> {args}.


takedown_env(_Args) -> ok.


test_is_node_id(_Args) ->
    ?assertEqual(false, quperl_octree_node_id:is_node_id(1)),

    ?assertEqual(true, quperl_octree_node_id:is_node_id(#ot_node_id{})),

    ?assertEqual(true, quperl_octree_node_id:is_node_id(quperl_octree_node_id:to_node_id([]))),

    ?assertEqual(false, quperl_octree_node_id:is_node_id(quperl_octree:new_volume())),
    ok.


test_children(_Args) ->
    Expected = [[1,2,0], [1,2,1], [1,2,2], [1,2,3], [1,2,4], [1,2,5], [1,2,6], [1,2,7]],

    Result = as_node_list(quperl_octree_node_id:children(quperl_octree_node_id:to_node_id([1,2]))),

    ?assertEqual(Expected, Result),

    ok.


test_get_depth() ->
    TestVals = [ {0, []}
               , {1, [1]}
               , {2, [1,2]}
               , {3, [1,2,3]}],

    lists:foreach(fun({D, L}) ->
            Val = quperl_octree_node_id:to_node_id(L),
            ?assertEqual(D, quperl_octree_node_id:get_depth(Val))
            end, TestVals),

    ok.


test_beyond() ->
    TestVals = [{[1],[1,2],[2]}],

    lists:foreach(fun({A,P,R}) ->
                          ANode = quperl_octree_node_id:to_node_id(A),
                          PNode = quperl_octree_node_id:to_node_id(P),
                          Ret = quperl_octree_node_id:to_node_list(quperl_octree_node_id:beyond(ANode, PNode)),
                          ?assertEqual(R, Ret)
                  end, TestVals),

    ok.


test_for_each_child() ->

    ?assertEqual([ [7,1,0],[7,1,1],[7,1,2],[7,1,3],
                   [7,1,4],[7,1,5],[7,1,6],[7,1,7]],
                 as_node_list(quperl_octree_node_id:for_each_child(quperl_octree_node_id:to_node_id([1]),
                                                           fun(C, E) ->
                                                                   [quperl_octree_node_id:prepend_node(E,C)]
                                                           end, 7))),
    ok.


test_is_ancestor_of() ->

    Tests = [{false, [1,0], [1,0]},
             {false, [0], [1,0]},
             {false, [2], [1,2]},
             {false, [4], [1,4]},
             {false, [7], [1,7]},
             {true, [1], [1,0]},
             {true, [2], [2,0]},
             {true, [4], [4,0]},
             {true, [7], [7,0]},
             {true, [], [7,0]},
             {false, [0,1], [1,0]},
             {true, [0,1], [0,1,0]},
             {true, [3,0,1], [3,0,1,0]},
             {true, [5,3,0,1], [5,3,0,1,6]},
             {true, [5,3], [5,3,0,1,6]},
             {false, [5,3,0,1], [5,3]},
             {false, [1,0,1], [1,0]}
            ],

    lists:foreach(fun({R, A, B}) ->
                          ?assertEqual(R, quperl_octree_node_id:is_ancestor_of(quperl_octree_node_id:to_node_id(A),
                                                       quperl_octree_node_id:to_node_id(B)))
                          end,
                  Tests),

    ok.

-dialyzer({nowarn_function, test_previous/0}).

test_previous() ->
    TestCases = [ {[1],[0]}
                , {[7],[6]}
                , {[1,1],[1,0]}
                , {[1,0],[0,7]}
                ],

    lists:foreach(fun({In, Out}) ->
                          R1 = quperl_octree_node_id:previous(quperl_octree_node_id:to_node_id(In)),
                          Result = quperl_octree_node_id:to_node_list(R1),
                          ?assertEqual(Out, Result)
                  end, TestCases),

        ?assertThrow(badargs, quperl_octree_node_id:previous(#ot_node_id{depth=0})),
        ?assertThrow(badargs, quperl_octree_node_id:previous(quperl_octree_node_id:to_node_id([0]))),
        ?assertThrow(badargs, quperl_octree_node_id:previous(quperl_octree_node_id:to_node_id([0,0,0]))),

    ok.


-dialyzer({nowarn_function, test_leaf/0}).

test_leaf() ->

%%     ?debugFmt("new_node_id([1,2,4]): ~p",[new_node_id([1,2,4])]),

    ?assertEqual(1,quperl_octree_node_id:leaf(quperl_octree_node_id:to_node_id([1]))),

    ?assertEqual(4,quperl_octree_node_id:leaf(quperl_octree_node_id:to_node_id([1,2,4]))),
    ?assertEqual(2,quperl_octree_node_id:leaf(quperl_octree_node_id:to_node_id([1,2,2]))),
    ?assertEqual(1,quperl_octree_node_id:leaf(quperl_octree_node_id:to_node_id([1,2,1]))),
    ?assertEqual(5,quperl_octree_node_id:leaf(quperl_octree_node_id:to_node_id([1,2,5]))),

    ?assertThrow(zero_depth, quperl_octree_node_id:leaf(#ot_node_id{depth=0})),

    ok.


-dialyzer({no_match, test_inner/0}).

test_inner() ->

%%     ?debugFmt("~n     [1,2,3]: ~p~ninner([1,2,3,4]): ~p",[new_node_id([1,2,3]),
%%                                                           inner(new_node_id([1,2,3,4]))]),
    ?assert(quperl_octree_node_id:is_equal(quperl_octree_node_id:to_node_id([]),
                                   quperl_octree_node_id:inner(quperl_octree_node_id:to_node_id([1])))),

    ?assert(quperl_octree_node_id:is_equal(quperl_octree_node_id:to_node_id([1,2,3]),
                                   quperl_octree_node_id:inner(quperl_octree_node_id:to_node_id([1,2,3,4])))),

    ?assertThrow(zero_depth, quperl_octree_node_id:inner(quperl_octree_node_id:to_node_id([]))),

    ok.


test_common_prefix() ->

        Inputs = [ {[0],[0],[0],[],[]}
                 , {[3,1,6],[3,1,2], [3,1],[6],[2]}
                 ],

    lists:foreach(fun({Point1,Point2,Pre,Post1,Post2}) ->

                          ?assertEqual({quperl_octree_node_id:to_node_id(Pre),
                                        quperl_octree_node_id:to_node_id(Post1),
                                        quperl_octree_node_id:to_node_id(Post2)},
                                       quperl_octree_node_id:common_prefix(quperl_octree_node_id:to_node_id(Point1), quperl_octree_node_id:to_node_id(Point2)))

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
                                  quperl_octree_node_id:common_prefix(quperl_octree_node_id:to_node_id(Position),
                                                              quperl_octree_node_id:to_node_id({P1 + B1, P2 + B2, P3 + B3})),
                              ?assertEqual(ExpectedAC, quperl_octree_node_id:to_node_list(Pre))

                      end, Inputs2),

    ok.


-dialyzer({nowarn_function, test_first_node/0}).

test_first_node() ->

    TestCases = [ {[1],1}
                , {[2],2}
                , {[4],4}
                , {[7],7}
                , {[0,1],0}
                ],

    lists:foreach(fun({List, Result}) ->
                          Node = quperl_octree_node_id:to_node_id(List, ?DEFAULT_MAX_DEPTH),
                          ?assertEqual(Result, quperl_octree_node_id:first_node(Node))
                          end, TestCases),

    ?assertThrow(zero_depth, quperl_octree_node_id:first_node(#ot_node_id{depth=0})),

    ok.

-dialyzer({no_match, test_is_equal/0}).

test_is_equal() ->
    NI1 = quperl_octree_node_id:to_node_id([], ?DEFAULT_MAX_DEPTH),
    ?assert(quperl_octree_node_id:is_equal(NI1, NI1)),

    NI2 = quperl_octree_node_id:to_node_id([1], ?DEFAULT_MAX_DEPTH),
    ?assert(quperl_octree_node_id:is_equal(NI2, NI2)),

    NI3 = quperl_octree_node_id:to_node_id([1,2,3,4,5], ?DEFAULT_MAX_DEPTH),
    ?assert(quperl_octree_node_id:is_equal(NI3, NI3)),

    ?assertNot(quperl_octree_node_id:is_equal(NI1, NI2)),
    ?assertNot(quperl_octree_node_id:is_equal(NI2, NI1)),

    ?assertNot(quperl_octree_node_id:is_equal(NI1, NI3)),
    ?assertNot(quperl_octree_node_id:is_equal(NI3, NI1)),

    NI4 = quperl_octree_node_id:to_node_id([4], ?DEFAULT_MAX_DEPTH),
    ?assert(quperl_octree_node_id:is_equal(NI4, NI4)),

    ?assertNot(quperl_octree_node_id:is_equal(NI2, NI4)),
    ?assertNot(quperl_octree_node_id:is_equal(NI4, NI2)),

    ok.

-dialyzer({no_match, test_rest_nodes/0}).

test_rest_nodes() ->

%%     ?debugFmt("~ndefault Depth:~n[2,3,4]: ~p~n[1,2,3,4]: ~p~nrest([1,2,3,4]): ~p",
%%               [to_node_id([2,3,4], ?DEFAULT_MAX_DEPTH),
%%                to_node_id([1,2,3,4], ?DEFAULT_MAX_DEPTH),
%%                rest_nodes(to_node_id([1,2,3,4], ?DEFAULT_MAX_DEPTH))]),

%%     ?debugFmt("~nDepth 64:~n[2,3,4]: ~p~n[1,2,3,4]: ~p~nrest([1,2,3,4]): ~p",
%%               [to_node_id([2,3,4], 64),
%%                to_node_id([1,2,3,4], 64),
%%                rest_nodes(to_node_id([1,2,3,4], 64))]),

    ?assert(quperl_octree_node_id:is_equal(quperl_octree_node_id:to_node_id([2,3,4], ?DEFAULT_MAX_DEPTH),
                     quperl_octree_node_id:rest_nodes(quperl_octree_node_id:to_node_id([1,2,3,4], ?DEFAULT_MAX_DEPTH)))),

    ?assertThrow(zero_depth, quperl_octree_node_id:rest_nodes(quperl_octree_node_id:to_node_id([], ?DEFAULT_MAX_DEPTH))),

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
                          ?assertEqual(quperl_octree_node_id:to_node_id(Result, ?DEFAULT_MAX_DEPTH),
                                       quperl_octree_node_id:append_node(quperl_octree_node_id:to_node_id(List,?DEFAULT_MAX_DEPTH),
                                                   New)),
                          ok
                  end, TestCases),

    ok.

test_specific_points() ->
    Tests = find_point_testcases(),

    lists:foreach(fun({Input, Expected}) ->
                          Result = quperl_octree_node_id:to_node_list(quperl_octree_node_id:to_node_id(Input,?DEFAULT_MAX_DEPTH)),
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
                         quperl_octree_node_id:to_node_list(quperl_octree_node_id:to_node_id(Val,?DEFAULT_MAX_DEPTH)))
                  end, TestVals),

    ok.


test_normalize_nodes() ->
    ?assertThrow(different_point_depth_not_supported,
                 quperl_octree_node_id:normalize(#ot_node_id{depth=1}, #ot_node_id{depth=2})),

    P1 = #ot_node_id{depth=62,x=3,y=2,z=5},
    P2 = #ot_node_id{depth=62,x=1,y=5,z=2},
    P1s = #ot_node_id{depth=62,x=1,y=2,z=2},
    P2s = #ot_node_id{depth=62,x=3,y=5,z=5},
    ?assertEqual({P1s, P2s}, quperl_octree_node_id:normalize(P1, P2)),

    ?assertThrow(unsupported_point_type_combination,
                 quperl_octree_node_id:normalize(P1, {0.2, 0.3, 0.4})),

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
                                       quperl_octree_node_id:normalize(A,B))
                  end, TestCases),

    ok.


test_to_node_id() ->

    ?assertThrow({badarg, _Val}, quperl_octree_node_id:to_node_id({0.0, 0.0, 1.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, quperl_octree_node_id:to_node_id({0.0, 1.0, 0.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, quperl_octree_node_id:to_node_id({1.0, 0.0, 0.0}, ?DEFAULT_MAX_DEPTH)),

    ?assertThrow({badarg, _Val}, quperl_octree_node_id:to_node_id({0.0, 0.0, -1.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, quperl_octree_node_id:to_node_id({0.0, -1.0, 0.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, quperl_octree_node_id:to_node_id({-1.0, 0.0, 0.0}, ?DEFAULT_MAX_DEPTH)),

    ?assertMatch([7,0,0|_], quperl_octree_node_id:to_node_list(quperl_octree_node_id:to_node_id({0.5, 0.5, 0.5},
                                                    ?DEFAULT_MAX_DEPTH))),

    ok.


test_default_max_depth() ->
    ?assertEqual(?DEFAULT_MAX_DEPTH, quperl_octree_node_id:default_max_depth()),

    ok.




test_bitlist() ->

    TestVals = [ {0, []}
               , {16#580000000000000, [2,4,5]}
               ],

    lists:foreach(fun({V, Exp}) ->
                          ?assertEqual(Exp, bitlist(V))
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
   [ quperl_octree_node_id:to_node_list(P1) | as_node_list(Rest)];

as_node_list([{P1, P2}|Rest]) ->
   [{quperl_octree_node_id:to_node_list(P1), quperl_octree_node_id:to_node_list(P2)} | as_node_list(Rest)].


%% %% bap/1
%% %% bit at pos (position is 0 - Max Depth)
%% -spec bap(Val :: non_neg_integer() | [non_neg_integer()] | {non_neg_integer(), non_neg_integer()}) -> non_neg_integer().
%% bap(P) -> bap(P, 0).
%%
%% bap({PStart, PEnd}, Acc) ->
%%     bap(lists:seq(PStart, PEnd), Acc);
%%
%% bap(P,Acc) when is_integer(P) -> Acc bor (1 bsl (?DEFAULT_MAX_DEPTH - P));
%%
%% bap(PList, Rest) when is_list(PList) ->
%%     lists:foldl(fun(P, Acc) ->
%%                         bap(P,Acc)
%%                         end, Rest, PList).


-spec bitlist(V :: non_neg_integer()) -> [non_neg_integer()].
bitlist(V) -> bitlist(V,?DEFAULT_MAX_DEPTH,[]).

bitlist(0, _, L) -> L;
bitlist(V, I, L) ->
      case (V band 1) of
          0 ->
              bitlist(V bsr 1, I - 1, L);
          1 ->
              bitlist(V bsr 1, I - 1, [I | L])
              end.

