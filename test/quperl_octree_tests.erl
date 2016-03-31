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
%%             is_all_ones, is_all_zeroes, split/3, 

-module(quperl_octree_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("quperl_octree.hrl").

-import(quperl_octree,[new_volume/0, new_volume/1, new_volume/2,

        normalize/2, to_node_id/2, to_node_list/1,
        box_to_volume/2, filter_full_area/1,common_prefix/2,
        is_equal/2, first_node/1, rest_nodes/1, append_node/2,
        xval/1, yval/1, zval/1, is_all_ones/1, is_all_zeroes/1,
        bit_count/1]).

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
                    ?_test(test_new_volume1(Args))
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
                    ]
      end }.

setup_env() -> {args}.

takedown_env(_Args) -> ok.

test_new_volume0(_Args) ->
    
    Volume = new_volume(),

    ?assertEqual(?DEFAULT_MAX_DEPTH, Volume#ot_volume.max_depth),
    
    ok.

test_new_volume1(_Args) -> 
    
    Volume = new_volume(10),

?assertEqual(10, Volume#ot_volume.max_depth),

ok.

test_is_all_zeroes() ->
    ?assertEqual(true, is_all_zeroes(to_node_id([], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(true, is_all_zeroes(to_node_id([0], ?DEFAULT_MAX_DEPTH))),
    
    ?assertEqual(false, is_all_zeroes(to_node_id([4], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, is_all_zeroes(to_node_id([2], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, is_all_zeroes(to_node_id([1], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, is_all_zeroes(to_node_id([7], ?DEFAULT_MAX_DEPTH))),
    
    ?assertError(function_clause, is_all_zeroes(0)),
    ok.

test_is_all_ones() ->
    ?assertEqual(false, is_all_ones(to_node_id([0], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, is_all_ones(to_node_id([4], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, is_all_ones(to_node_id([2], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, is_all_ones(to_node_id([1], ?DEFAULT_MAX_DEPTH))),

    %% matter of definition: the required number of 0's for a path of 0 is 0!
    ?assertEqual(true, is_all_ones(to_node_id([], ?DEFAULT_MAX_DEPTH))),

    ?assertEqual(false, is_all_ones(to_node_id([0,7], ?DEFAULT_MAX_DEPTH))),

    ?assertEqual(true, is_all_ones(to_node_id([7], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(true, is_all_ones(to_node_id([7,7], ?DEFAULT_MAX_DEPTH))),
    
    ?assertError(function_clause, is_all_ones(3)),
    ok.

test_node_id_bit_count() ->
    ?assertEqual(1, bit_count(#ot_node_id{depth=1, x=1, y =0, z=0})),
    ?assertEqual(1, bit_count(#ot_node_id{depth=1, x=0, y =1, z=0})),
    ?assertEqual(1, bit_count(#ot_node_id{depth=1, x=0, y =0, z=1})),
    ?assertEqual(3, bit_count(#ot_node_id{depth=1, x=1, y =1, z=1})),
    
    Val = (1 bsl 63) + 1,
    ?assertEqual(6, bit_count(#ot_node_id{depth=64, x=Val, y=Val, z=Val})),

    ok.

test_first_node() ->
    
    TestCases = [ {[1],1}
                , {[2],2}
                , {[4],4}
                , {[7],7}
                , {[0,1],0}
                ],
    
    lists:foreach(fun({List, Result}) ->
                          Node = to_node_id(List, ?DEFAULT_MAX_DEPTH),
                          ?assertEqual(Result, first_node(Node))
                          end, TestCases),
    
    ?assertThrow(zero_depth, first_node(#ot_node_id{depth=0})),

    ok.

test_is_equal() ->
    NI1 = to_node_id([], ?DEFAULT_MAX_DEPTH),
    ?assert(is_equal(NI1, NI1)),

    NI2 = to_node_id([1], ?DEFAULT_MAX_DEPTH),
    ?assert(is_equal(NI2, NI2)),

    NI3 = to_node_id([1,2,3,4,5], ?DEFAULT_MAX_DEPTH),
    ?assert(is_equal(NI3, NI3)),

    ?assertNot(is_equal(NI1, NI2)),
    ?assertNot(is_equal(NI2, NI1)),

    ?assertNot(is_equal(NI1, NI3)),
    ?assertNot(is_equal(NI3, NI1)),

    NI4 = to_node_id([4], ?DEFAULT_MAX_DEPTH),
    ?assert(is_equal(NI4, NI4)),

    ?assertNot(is_equal(NI2, NI4)),
    ?assertNot(is_equal(NI4, NI2)),

    ok.

test_rest_nodes() ->

    ?debugFmt("~ndefault Depth:~n[2,3,4]: ~p~n[1,2,3,4]: ~p~nrest([1,2,3,4]): ~p",
              [to_node_id([2,3,4], ?DEFAULT_MAX_DEPTH),
               to_node_id([1,2,3,4], ?DEFAULT_MAX_DEPTH),
               rest_nodes(to_node_id([1,2,3,4], ?DEFAULT_MAX_DEPTH))]),

    ?debugFmt("~nDepth 64:~n[2,3,4]: ~p~n[1,2,3,4]: ~p~nrest([1,2,3,4]): ~p",
              [to_node_id([2,3,4], 64),
               to_node_id([1,2,3,4], 64),
               rest_nodes(to_node_id([1,2,3,4], 64))]),

    ?assert(is_equal(to_node_id([2,3,4], ?DEFAULT_MAX_DEPTH),
                     rest_nodes(to_node_id([1,2,3,4], ?DEFAULT_MAX_DEPTH)))),
    
    ?assertThrow(zero_depth, rest_nodes(to_node_id([], ?DEFAULT_MAX_DEPTH))),

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
                          ?assertEqual(to_node_id(Result, ?DEFAULT_MAX_DEPTH), 
                                       append_node(to_node_id(List,?DEFAULT_MAX_DEPTH),
                                                   New)),
                          ok
                  end, TestCases),

    ok.

test_specific_points() ->
    Tests = find_point_testcases(),

    lists:foreach(fun({Input, Expected}) ->
                          Result = to_node_list(to_node_id(Input,?DEFAULT_MAX_DEPTH)),
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
                         to_node_list(to_node_id(Val,?DEFAULT_MAX_DEPTH)))
                  end, TestVals),

    ok.


test_normalize_nodes() ->
    ?assertThrow(different_point_depth_not_supported, 
                 normalize(#ot_node_id{depth=1}, #ot_node_id{depth=2})),
    
    P1 = #ot_node_id{depth=62,x=3,y=2,z=5},
    P2 = #ot_node_id{depth=62,x=1,y=5,z=2},
    P1s = #ot_node_id{depth=62,x=1,y=2,z=2},
    P2s = #ot_node_id{depth=62,x=3,y=5,z=5},
    ?assertEqual({P1s, P2s}, normalize(P1, P2)),

    ?assertThrow(unsupported_point_type_combination, 
                 normalize(P1, {0.2, 0.3, 0.4})),

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
                                       normalize(A,B))
                  end, TestCases),

    ok.


test_to_node_id() ->
        
    ?assertThrow({badarg, _Val}, to_node_id({0.0, 0.0, 1.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, to_node_id({0.0, 1.0, 0.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, to_node_id({1.0, 0.0, 0.0}, ?DEFAULT_MAX_DEPTH)),

    ?assertThrow({badarg, _Val}, to_node_id({0.0, 0.0, -1.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, to_node_id({0.0, -1.0, 0.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, to_node_id({-1.0, 0.0, 0.0}, ?DEFAULT_MAX_DEPTH)),

    ?assertMatch([7,0,0|_], to_node_list(to_node_id({0.5, 0.5, 0.5}, 
                                                    ?DEFAULT_MAX_DEPTH))),

    ok. 
