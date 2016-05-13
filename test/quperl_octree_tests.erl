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

-export([test_new_box_to_volume/0]).
%%
%% Fixtures
%%


api_test_() ->
    { "provide minimal api tests",
      setup,
      fun setup_env/0,
      fun takedown_env/1,
      fun(Args) -> [ ?_test(test_new_volume0(Args))
                   , ?_test(test_new_volume1(Args))
%%                    , ?_test(test_new_volume2(Args))
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
                    , ?_test(test_left_wall())
                    , ?_test(test_left_wall_at())
                    , ?_test(test_right_wall())
                    , ?_test(test_right_wall_at())
                    , ?_test(test_default_max_depth())
                    , ?_test(test_xor_dim())
%%                     , ?_test(test_split())
                    , ?_test(test_inner())
                    , ?_test(test_leaf())
                    , ?_test(test_previous())
                    , ?_test(test_sweep())
                    , ?_test(test_common_prefix())
                    , ?_test(test_filter_full_area())
%%                     , ?_test(test_box_to_volume2())
%%                     , ?_test(test_box_to_volume1())
                    , ?_test(test_for_each_child())
                    , ?_test(test_is_ancestor_of())
                    , ?_test(test_beyond())
                    , ?_test(test_get_code_at())
                    , ?_test(test_get_depth())
                    , ?_test(test_handle_node_parent())
                    , ?_test(test_make_sub_node_points())
                    , ?_test(test_right_wall_calc())
                    , ?_test(test_left_wall_calc())
                    , ?_test(test_bitlist())
                    , ?_test(test_new_box_to_volume())
                    ]
      end }.

setup_env() -> {args}.

takedown_env(_Args) -> ok.

test_new_volume0(_Args) ->

    Volume = quperl_octree:new_volume(),

    ?assertEqual(?DEFAULT_MAX_DEPTH, Volume#ot_volume.max_depth),

    ok.

test_new_volume1(_Args) ->

                          InNodes = lists:map(fun(P1) -> quperl_octree:to_node_id(P1) end,
                                              [[1],[2]]),

    Volume = quperl_octree:new_volume(InNodes),

?assertEqual(2, length(Volume#ot_volume.spaces)),

ok.

test_new_volume2(_Args) ->

    TestCases = [{[],{0.479, 0.499, 0.499}, {0.501, 0.501, 0.501}},
                 {[],{0.479, 0.479, 0.499}, {0.501, 0.501, 0.501}},
                 {[],{0.479, 0.499, 0.479}, {0.501, 0.501, 0.501}},
                 {[],{0.499, 0.499, 0.499}, {0.501, 0.501, 0.501}},
                 {{badarg, val}, {0.0, 0.0, 0.0}, {2.0, 2.0, 2.0}}],

    lists:foreach(fun({R,P1, P2}) ->
                          N1 = quperl_octree:to_node_id(P1),
                          N2 = quperl_octree:to_node_id(P2),
                          L1 = quperl_octree:box_to_volume([{N1,N2}],[]),
                          ?debugFmt("~nP1: ~p~nP2: ~p~nbox: ~p",
                                    [quperl_octree:to_node_list(N1),
                                     quperl_octree:to_node_list(N2),
                                     L1]),
                          ?assertEqual(R, catch quperl_octree:new_volume(P1, P2))
                          end, TestCases),

    ok.


test_new_box_to_volume() ->

    TestVals = [
                 %% a single point should return a single point
                 {{0.499, 0.499, 0.499},
                  {0.499, 0.499, 0.499},
                  [quperl_octree:to_node_list(quperl_octree:to_node_id({0.499, 0.499, 0.499}))]}

               , {[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                  [7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7],
                  [[]]} %% 0-length node, as this is the whole cube

%%                , {[0,0,0],
%%                   [7,7,6],
%%                   [[]]}

%%                 {[0,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
%%                    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
%%                    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7],
%%                   [7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%%                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%%                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
%%                   []}

%%                , {[0,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
%%                    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
%%                    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7],
%%                   [7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%%                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%%                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
%%                   []}

%%                , {{0.499, 0.499, 0.499},
%%                   {0.501, 0.501, 0.501},
%%                   []}
               ],


    lists:foreach(fun({P1, P2, Expected}) ->
                          Result = quperl_octree:new_box_to_volume(quperl_octree:to_node_id(P1), 
                                                                   quperl_octree:to_node_id(P2)),
                          PrepResult = as_node_list(Result),
                          ?debugVal(PrepResult),
                          ?debugVal(length(Result)),
                          ?assertEqual(Expected, PrepResult)
                  end, TestVals),
    ok.


%%
%% Test cases 2-9: consider nodes of two timensions (z is set analogous)
%%       -> y
%%    --------------    --------------
%%  | |I    |II    |    |V    |VI    |
%% \/ | *p1 |      |    |     |      |  P1: [0,0,7,7,0]
%%  x |     |      |    |     |      |  P2: [0,7,7,0,7]
%%    --------------    --------------
%%    |III  |IV    |    |VII  |VIII  |
%%    |     |      |    |     |  *p2 |
%%    |     |      |    |     |      |
%%    --------------    --------------
%%
%% Test cases 10-11: z is set to 0
%%    --------------
%%    |I    |II    |
%%    | *p1 |      |  P1: [0,0,0,6,0]
%%    |     |   *p2|  P2: [0,2,6,6,0]
%%    --------------
%%    |III  |IV    | Quadrant III amd IV have a delta in less than 3 dimensions, thus no volume
%%    |     |      |
%%    |     |      |
%%    --------------
%%
%% Test cases 12: z is set to 0
%%    --------------
%%    |I*p1 |II    |
%%    |     |      |  P1: [0,0,0,6,0]
%%    |  p2*|      |  P2: [0,0,6,6,0]
%%    --------------
%%    |III  |IV    |
%%    |     |      |
%%    |     |      |
%%    --------------
%%
%% Variations: 
%%  13: like 1, but with P1 in the upper node at the given level
%%  14: like 8, but with P2 in the lower node at the given level
%%  15: like 1, but with a longer prefix

test_make_sub_node_points() ->
    TestVals = [ {false, false,0,false,1,x,[1,1,2],[1,6,7], {badargs,false,false,0}},
                 {false, false,0,true,1,x,[1,1,2],[1,6,7], {badargs,false,false,0}}

               , {true,  false, 1, false, 2, x, [0,0,7,7,0],[0,7,7,0,7], {[3,4],[3,4,5]}}   %% Quadrant I
               , {true,  false, 1, false, 2, y, [0,0,7,7,0],[0,7,7,0,7], {[3,4],[3,4,5]}}
               , {true,  false, 1, false, 2, z, [0,0,7,7,0],[0,7,7,0,7], {[3,4],[3,4,5]}}

               , {false, false, 1, false, 2, x, [0,0,7,7,0],[0,7,7,0,7], {[3,4],[3,4,5]}}   %% Quadrant II
               , {false, false, 1, true,  2, y, [0,0,7,7,0],[0,7,7,0,7], {[2],[2,3,5]}}
               , {false, false, 1, false, 2, z, [0,0,7,7,0],[0,7,7,0,7], {[3,4],[3,4,5]}}

               , {false, false, 1, true,  2, x, [0,0,7,7,0],[0,7,7,0,7], {[2],[2,3,5]}}   %% Quadrant III
               , {false, false, 1, false, 2, y, [0,0,7,7,0],[0,7,7,0,7], {[3,4],[3,4,5]}}
               , {false, false, 1, false, 2, z, [0,0,7,7,0],[0,7,7,0,7], {[3,4],[3,4,5]}}

               , {false, false, 1, true,  2, x, [0,0,7,7,0],[0,7,7,0,7], {[2],[2,3,5]}}   %% Quadrant IV
               , {false, false, 1, true,  2, y, [0,0,7,7,0],[0,7,7,0,7], {[2],[2,3,5]}}
               , {false, false, 1, false, 2, z, [0,0,7,7,0],[0,7,7,0,7], {[3,4],[3,4,5]}}

               , {false, false, 1, false, 2, x, [0,0,7,7,0],[0,7,7,0,7], {[3,4],[3,4,5]}}   %% Quadrant V
               , {false, false, 1, false, 2, y, [0,0,7,7,0],[0,7,7,0,7], {[3,4],[3,4,5]}}
               , {false, false, 1, true,  2, z, [0,0,7,7,0],[0,7,7,0,7], {[2],[2,3,5]}}

               , {false, false, 1, true,  2, x, [0,0,7,7,0],[0,7,7,0,7], {[2],[2,3,5]}}   %% Quadrant VI
               , {false, false, 1, false, 2, y, [0,0,7,7,0],[0,7,7,0,7], {[3,4],[3,4,5]}}
               , {false, false, 1, true,  2, z, [0,0,7,7,0],[0,7,7,0,7], {[2],[2,3,5]}}

               , {false, false, 1, false, 2, x, [0,0,7,7,0],[0,7,7,0,7], {[3,4],[3,4,5]}}   %% Quadrant VII
               , {false, false, 1, true,  2, y, [0,0,7,7,0],[0,7,7,0,7], {[2],[2,3,5]}}
               , {false, false, 1, true,  2, z, [0,0,7,7,0],[0,7,7,0,7], {[2],[2,3,5]}}

               , {false, true,  1, true,  2, x, [0,0,7,7,0],[0,7,7,0,7], {[2],[2,3,5]}}   %% Quadrant VIII
               , {false, true,  1, true,  2, y, [0,0,7,7,0],[0,7,7,0,7], {[2],[2,3,5]}}
               , {false, true,  1, true,  2, z, [0,0,7,7,0],[0,7,7,0,7], {[2],[2,3,5]}}
%% Testcases 10-11
               , {true,  false, 0, false, 2, x, [0,0,0,6,0],[0,2,6,6,0], {[4],[3,4]}}   %% Quadrant I
               , {true,  false, 1, false, 2, y, [0,0,0,6,0],[0,2,6,6,0], {[4],[3,4,5]}}
               , {true,  false, 0, false, 2, z, [0,0,0,6,0],[0,2,6,6,0], {[],[]}}

               , {false, true,  0, false, 2, x, [0,0,0,6,0],[0,2,6,6,0], {[4],[3,4]}}   %% Quadrant II
               , {false, true,  1, true,  2, y, [0,0,0,6,0],[0,2,6,6,0], {[2],[2,3,4]}}
               , {false, true,  0, false, 2, z, [0,0,0,6,0],[0,2,6,6,0], {[],[]}}

%% Testcase 12 (should not happen, as caught by higher level)
%%                , {true,  true,  0, false, 2, x, [0,0,0,6,0],[0,0,6,6,0], {[4],[3,4]}}   %% Quadrant II
%%                , {true,  true,  0, true,  2, y, [0,0,0,6,0],[0,0,6,6,0], {[2],[2,3,4]}}
%%                , {true,  true,  0, false, 2, z, [0,0,0,6,0],[0,2,6,6,0], {[],[]}}

%% Testcase 15: (node is [0,0,7])
               , {true,  false, 1, false, 4, x, [0,0,7,0,0,7,7,0],[7,0,7,0,7,7,0,7], {[3,6,7],[3,5,6,7,8]}}   %% Quadrant I
               , {true,  false, 1, false, 4, y, [0,0,7,0,0,7,7,0],[7,0,7,0,7,7,0,7], {[3,6,7],[3,5,6,7,8]}}
               , {true,  false, 1, false, 4, z, [0,0,7,0,0,7,7,0],[7,0,7,0,7,7,0,7], {[3,6,7],[3,5,6,7,8]}}

%% Testcase 13 (node is [0]):
               , {true,  false, 1, true,  2, x, [0,7,7,7,0],[7,0,7,0,7], {[2,3,4],[3,4,5]}}   %% Quadrant I
               , {true,  false, 1, true,  2, y, [0,7,7,7,0],[7,0,7,0,7], {[2,3,4],[3,4,5]}}
               , {true,  false, 1, true,  2, z, [0,7,7,7,0],[7,0,7,0,7], {[2,3,4],[3,4,5]}}

%% Testcase 14 (node is [7]):
               , {false, true,  1, false,  2, x, [0,7,7,7,0],[7,0,7,0,7], {[1,2],[1,3,5]}}   %% Quadrant I
               , {false, true,  1, false,  2, y, [0,7,7,7,0],[7,0,7,0,7], {[1,2],[1,3,5]}}
               , {false, true,  1, false,  2, z, [0,7,7,7,0],[7,0,7,0,7], {[1,2],[1,3,5]}}

],

    lists:foreach(fun({I1, I2, Delta, Upper, Depth, Dim, P1, P2, Expected}) ->
                          P1Id = quperl_octree:to_node_id(P1),
                          P2Id = quperl_octree:to_node_id(P2),

                          Result = (catch quperl_octree:make_sub_node_points(I1, I2,
                                                                                 Delta, Upper,
                                                                                 Depth, Dim,
                                                                                 P1Id, P2Id)),

                          ResultPrep = case Result of
                                           {V1, V2} ->
                                               R1 = bitlist(V1),
                                               R2 = bitlist(V2),
                                               io:format("Result is: {~p,~p}~n",[R1,R2]),
                                               {R1,R2};
                                           Other -> Other
                                       end,

                          case Expected of
                              {E1, E2} ->
                                  io:format("Expected is: {~p,~p}~n~n",[E1,E2]);
                              _ -> Expected
                                  end,

                          ?assertEqual(Expected, ResultPrep)
                  end, TestVals),

    ok.

test_handle_node_parent() ->
    TestVals = [ {false, false, 0,0,0,[1],[0,1],[0,2], []}  %% not in field
               , {true,  false, 0,0,0,[1],[0,1],[0,2], []}  %% no volume
               , {true,  false, 0,0,1,[1],[0,1],[0,2], []}  %%    :
               , {true,  false, 0,1,1,[1],[0,1],[0,2], []}  %%    :
               , {false, true,  0,0,0,[1],[0,1],[0,2], []}  %%    :
               , {false, true,  0,0,1,[1],[0,1],[0,2], []}  %%    :
               , {false, true,  0,1,1,[1],[0,1],[0,2], []}  %%    :
               , {true,  true,  0,0,0,[0,7], [0,7,7,7,0],[0,7,7,7,7], [[0,7,7,7]]}
               
%% Exception: {badargs,unkown_combination,
%%                     {true,false,1,true,3,x,
%%                           [0,7,7,7,7...],
%%                           [7,0,7,7,7...]}}
%% Node: [0,7]
%% Point1: [0,7,7,7,7,7,7,7,7,0,7,7,7,7,7,0,0,7,7,7,0,7,7,0,7,7,0,0,7,0,0,0,7,0,
%%          7,7,0,7,0,0,0,0,7,7,7,0,0,7,0,7,0,7,7,0,0,0,0,0,0,0]
%% Point2: [7,0,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
%%          7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7]
%% Exception: {badargs,unkown_combination,
%%                     {false,true,1,false,3,x,
%%                            [0,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%%                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%%                             0,0,0,0,0,0,0,0,0,0],
%%                            [7,0,0,0,0,0,0,0,0,7,0,0,0,0,0,7,7,0,0,0,7,0,0,7,0,
%%                             0,7,7,0,7,7,7,0,7,0,0,7,0,7,7,7,7,0,0,0,7,7,0,7,0,
%%                             7,0,7,0,0,0,0,0,0,0]}}
%% Node: [7,0]
%% Point1: [0,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%%          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
%% Point2: [7,0,0,0,0,0,0,0,0,7,0,0,0,0,0,7,7,0,0,0,7,0,0,7,0,0,7,7,0,7,7,7,0,7,
%%          0,0,7,0,7,7,7,7,0,0,0,7,7,0,7,0,7,0,7,0,0,0,0,0,0,0]

               ],

    lists:foreach(fun({I1, I2, Dx, Dy, Dz, Node, P1, P2, Expected}) ->
                          NodeId = quperl_octree:to_node_id(Node),
                          P1Id = quperl_octree:to_node_id(P1),
                          P2Id = quperl_octree:to_node_id(P2),

                          Result = as_node_list(quperl_octree:handle_node_parent(I1, I2,
                                                                                 Dx, Dy, Dz,
                                                                                 NodeId,
                                                                                 P1Id, P2Id)),

                          ?assertEqual(Expected, Result)
                  end, TestVals),
    ok.

-spec set_value(Val :: non_neg_integer(), Dim :: x|y|z, Depth :: non_neg_integer()) -> #ot_node_id{}.
set_value(Val, x, Depth) -> #ot_node_id{depth = Depth, x = Val};
set_value(Val, y, Depth) -> #ot_node_id{depth = Depth, y = Val};
set_value(Val, z, Depth) -> #ot_node_id{depth = Depth, z = Val}.

% only use the z values
test_right_wall_calc() ->
    TestVals = [ {{[1,0],1},[0,1]}
               , {{[0,1,0],1},[0,1,1]}
               , {{[1,1,0],1},[0,1,1]}
               , {{[0,1,0,1],2},[0,0,1,1]}
               , {{[0,0,0,1],2},[0,0,1,1]}
               , {{[1,0,0,1],2},[1,0,1,1]}
               ],

    lists:foreach(fun({{L, Depth}, Expected}) ->
                          Node = quperl_octree:to_node_id(L),
                          Val = quperl_octree:get_value(Node, z),
                          ND = quperl_octree:get_depth(Node),

                          Result = quperl_octree:right_wall_calc(Val, ND, Depth),
                          ?assertEqual(Expected, quperl_octree:to_node_list(set_value(Result, z, ND)))
                  end, TestVals),

    ok.


% only use the z values
test_left_wall_calc() ->
    TestVals = [ {{[1,0],1},[1,0]}
               , {{[0,1,0],1},[1,0,0]}
               , {{[1,1,0],1},[1,0,0]}
               , {{[0,1,0,1],2},[0,1,0,0]}
               , {{[0,0,0,1],2},[0,1,0,0]}
               , {{[1,0,0,1],2},[1,1,0,0]}
               ],

    lists:foreach(fun({{L, Depth}, Expected}) ->
                          Node = quperl_octree:to_node_id(L),
                          Val = quperl_octree:get_value(Node, z),
                          ND = quperl_octree:get_depth(Node),

                          Result = quperl_octree:left_wall_calc(Val, Depth),
                          ?assertEqual(Expected, quperl_octree:to_node_list(set_value(Result, z, ND)))
                  end, TestVals),

    ok.

test_get_code_at() ->
    TestVals = [ {1, [1], 1}
               , {2, [1,2], 2}
               , {2, [1,2,3], 2}
               , {3, [1,2,3], 3}],

    lists:foreach(fun({D, L, R}) ->
            Val = quperl_octree:to_node_id(L),
            ?assertEqual(R, quperl_octree:get_code_at(D, Val))
            end, TestVals),

    ok.

test_get_depth() ->
    TestVals = [ {0, []}
               , {1, [1]}
               , {2, [1,2]}
               , {3, [1,2,3]}],

    lists:foreach(fun({D, L}) ->
            Val = quperl_octree:to_node_id(L),
            ?assertEqual(D, quperl_octree:get_depth(Val))
            end, TestVals),

    ok.


test_beyond() ->
    TestVals = [{[1],[1,2],[2]}],

    lists:foreach(fun({A,P,R}) ->
                          ANode = quperl_octree:to_node_id(A),
                          PNode = quperl_octree:to_node_id(P),
                          Ret = quperl_octree:to_node_list(quperl_octree:beyond(ANode, PNode)),
                          ?assertEqual(R, Ret)
                  end, TestVals),

    ok.


test_for_each_child() ->

    ?assertEqual([ [7,1,0],[7,1,1],[7,1,2],[7,1,3],
                   [7,1,4],[7,1,5],[7,1,6],[7,1,7]],
                 as_node_list(quperl_octree:for_each_child(quperl_octree:to_node_id([1]),
                                                           fun(C, E) ->
                                                                   [quperl_octree:prepend_node(E,C)]
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
                          ?assertEqual(R, quperl_octree:is_ancestor_of(quperl_octree:to_node_id(A),
                                                       quperl_octree:to_node_id(B)))
                          end,
                  Tests),

    ok.


test_previous() ->
    TestCases = [ {[1],[0]}
                , {[7],[6]}
                , {[1,1],[1,0]}
                , {[1,0],[0,7]}
                ],

    lists:foreach(fun({In, Out}) ->
                          R1 = quperl_octree:previous(quperl_octree:to_node_id(In)),
                          Result = quperl_octree:to_node_list(R1),
                          ?assertEqual(Out, Result)
                  end, TestCases),

        ?assertThrow(badargs, quperl_octree:previous(#ot_node_id{depth=0})),
        ?assertThrow(badargs, quperl_octree:previous(quperl_octree:to_node_id([0]))),
        ?assertThrow(badargs, quperl_octree:previous(quperl_octree:to_node_id([0,0,0]))),

    ok.


test_leaf() ->

%%     ?debugFmt("new_node_id([1,2,4]): ~p",[new_node_id([1,2,4])]),

    ?assertEqual(1,quperl_octree:leaf(quperl_octree:to_node_id([1]))),

    ?assertEqual(4,quperl_octree:leaf(quperl_octree:to_node_id([1,2,4]))),
    ?assertEqual(2,quperl_octree:leaf(quperl_octree:to_node_id([1,2,2]))),
    ?assertEqual(1,quperl_octree:leaf(quperl_octree:to_node_id([1,2,1]))),
    ?assertEqual(5,quperl_octree:leaf(quperl_octree:to_node_id([1,2,5]))),

    ?assertThrow(zero_depth, quperl_octree:leaf(#ot_node_id{depth=0})),

    ok.


test_inner() ->

%%     ?debugFmt("~n     [1,2,3]: ~p~ninner([1,2,3,4]): ~p",[new_node_id([1,2,3]),
%%                                                           inner(new_node_id([1,2,3,4]))]),
    ?assert(quperl_octree:is_equal(quperl_octree:to_node_id([]),
                                   quperl_octree:inner(quperl_octree:to_node_id([1])))),

    ?assert(quperl_octree:is_equal(quperl_octree:to_node_id([1,2,3]),
                                   quperl_octree:inner(quperl_octree:to_node_id([1,2,3,4])))),

    ?assertThrow(zero_depth, quperl_octree:inner(quperl_octree:to_node_id([]))),

    ok.


test_box_to_volume1() ->

    TestVals = [ { [{ [1,0], [1,7] }],
                   [[1]]
                 },
                 { [{ {0.479, 0.499, 0.499}, {0.501, 0.501, 0.501} }],
                   [[1]]
                 }
               ],

    lists:foreach(fun({L, Out}) ->
                          InList = lists:map(fun({P1, P2}) -> {quperl_octree:to_node_id(P1),
                                                               quperl_octree:to_node_id(P2)} end, L),

                          ?assertEqual(Out, as_node_list(quperl_octree:box_to_volume(InList)))
                  end, TestVals),

ok.


test_box_to_volume2() ->

    TestVals = [ {[], [quperl_octree:to_node_id([1])], [[1]]}
               , {[{quperl_octree:to_node_id([1,0]),
                    quperl_octree:to_node_id([1,7])}], [],
                  [[1,0],[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[1,7]]}
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
              {[[1,0],[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[2,0]],
               [[1,0],[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[2,0]]},
              {[[1,0,1],[1,1,1],[1,2,1],[1,3,1],[1,4,1],[1,5,1],[1,6,1],[1,7,1]],
               [[1,0,1],[1,1,1],[1,2,1],[1,3,1],[1,4,1],[1,5,1],[1,6,1],[1,7,1]]},
              {[[1,0,0],[1,1,0],[1,2,0],[1,3,0],[1,4,0],[1,5,0],[1,6,0],[1,7,0]],
               [[1,0,0],[1,1,0],[1,2,0],[1,3,0],[1,4,0],[1,5,0],[1,6,0],[1,7,0]]},
              {[[1,0,0],[1,1,1],[1,2,2],[1,3,3],[1,4,4],[1,5,5],[1,6,6],[1,7,7]],
               [[1,0,0],[1,1,1],[1,2,2],[1,3,3],[1,4,4],[1,5,5],[1,6,6],[1,7,7]]},
              {[[1,0,0],[1,1,1],[1,2,2],[1,3,3],[1,3,4],[1,5,5],[1,6,6],[1,7,7]],
               [[1,0,0],[1,1,1],[1,2,2],[1,3,3],[1,3,4],[1,5,5],[1,6,6],[1,7,7]]}
             ],

    lists:foreach(fun({In, Expected}) ->
                          InList = lists:map(fun quperl_octree:to_node_id/1, In),
                          Result = as_node_list(quperl_octree:sweep(InList)),
                          ?assertEqual(Expected, Result)
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


%%  set all the bits but the first to 1
test_right_wall() ->

    TestVals = [ {[],[],[],[]}
               , {[0], [0], [0], [0]}
               , {[0,0], [0,4], [0,2], [0,1]}
               , {[5,2,6], [1,6,6], [5,2,6], [4,3,7]}
               , {[7], [3], [5], [6]}
               ],

    lists:foreach(fun({I, XL, YL, ZL}) ->
                          N = quperl_octree:to_node_id(I),

                          ?assertEqual(quperl_octree:to_node_id(XL),
                                       quperl_octree:right_wall(N,x)),

                          ?assertEqual(quperl_octree:to_node_id(YL),
                                       quperl_octree:right_wall(N,y)),

                          ?assertEqual(quperl_octree:to_node_id(ZL),
                                       quperl_octree:right_wall(N,z))
                          end, TestVals),

    ok.


%%  set all the bits but the first to 1
test_right_wall_at() ->

    TestVals = [ {1, [],[],[],[]}
               , {1, [0], [0], [0], [0]}
               , {1, [0,0], [0,4], [0,2], [0,1]}
               , {2, [1,0,0], [1,0,4], [1,0,2], [1,0,1]}
               , {2, [2,0,0], [2,0,4], [2,0,2], [2,0,1]}
               , {2, [4,0,0], [4,0,4], [4,0,2], [4,0,1]}
               , {1, [5,2,6], [1,6,6], [5,2,6], [4,3,7]}
               , {3, [5,7,5,2,6], [5,7,1,6,6], [5,7,5,2,6], [5,7,4,3,7]}
               , {3, [0,7,7], [0,7,3], [0,7,5], [0,7,6]}
               ],

    lists:foreach(fun({D, I, XL, YL, ZL}) ->
                          N = quperl_octree:to_node_id(I),

                          ?assertEqual(XL,
                                       quperl_octree:to_node_list(quperl_octree:right_wall(N,D,x))),
                          ?assertEqual(YL,
                                       quperl_octree:to_node_list(quperl_octree:right_wall(N,D,y))),
                          ?assertEqual(ZL,
                                       quperl_octree:to_node_list(quperl_octree:right_wall(N,D,z)))

                          end, TestVals),

    ok.


%%  set all the bits but the first to 0
test_left_wall() ->

    TestVals = [ %% invalid depth: {[], [], [], []}
                 {[0], [4], [2], [1]}
               , {[0,7], [4,3], [2,5], [1,6]}
               , {[5,2,6],[5,2,2],[7,0,4],[5,2,6]}
               ],

    lists:foreach(fun({I, XL, YL, ZL}) ->
                          N = quperl_octree:to_node_id(I),

                          ?assertEqual(quperl_octree:to_node_id(XL),
                                       quperl_octree:left_wall(N,x)),

                          ?assertEqual(quperl_octree:to_node_id(YL),
                                       quperl_octree:left_wall(N,y)),

                          ?assertEqual(quperl_octree:to_node_id(ZL),
                                       quperl_octree:left_wall(N,z))
                          end, TestVals),

    ok.

%%  set all the bits but the first to 0, starting at a given depth
test_left_wall_at() ->

    TestVals = [ %% invalid depth: {[], [], [], []}
                 {1, [0], [4], [2], [1]}
               , {1, [0,7], [4,3], [2,5], [1,6]}
               , {1, [5,2,6],[5,2,2],[7,0,4],[5,2,6]}
               , {2, [1,0], [1,4], [1,2], [1,1]}
               , {3, [4,7,0,7], [4,7,4,3], [4,7,2,5], [4,7,1,6]}
               , {4, [1,2,4,5,2,6],[1,2,4,5,2,2],[1,2,4,7,0,4],[1,2,4,5,2,6]}
               ],

    lists:foreach(fun({D, I, XL, YL, ZL}) ->
                          N = quperl_octree:to_node_id(I),

                          ?assertEqual(XL,
                                       quperl_octree:to_node_list(quperl_octree:left_wall(N,D,x))),

                          ?assertEqual(YL,
                                       quperl_octree:to_node_list(quperl_octree:left_wall(N,D,y))),

                          ?assertEqual(ZL,
                                       quperl_octree:to_node_list(quperl_octree:left_wall(N,D,z)))

                          end, TestVals),

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
   [ quperl_octree:to_node_list(P1) | as_node_list(Rest)];

as_node_list([{P1, P2}|Rest]) ->
   [{quperl_octree:to_node_list(P1), quperl_octree:to_node_list(P2)} | as_node_list(Rest)].


%% bap/1
%% bit at pos (position is 0 - Max Depth)
-spec bap(Val :: non_neg_integer() | [pos_integer()] | {pos_integer(), pos_integer()}) -> non_neg_integer().
bap(P) -> bap(P, 0).

bap({PStart, PEnd}, Acc) ->
    bap(lists:seq(PStart, PEnd), Acc);

bap(P,Acc) when is_integer(P) -> Acc bor (1 bsl (?DEFAULT_MAX_DEPTH - P));

bap(PList, Acc) when is_list(PList) ->
    lists:foldl(fun(P, Acc) ->
                        bap(P,Acc)
                        end, Acc, PList).


-spec bitlist(V :: non_neg_integer()) -> [pos_integer()].
bitlist(V) -> bitlist(V,?DEFAULT_MAX_DEPTH,[]).

bitlist(0, _, L) -> L;
bitlist(V, I, L) ->
      case (V band 1) of
          0 ->
              bitlist(V bsr 1, I - 1, L);
          1 ->
              bitlist(V bsr 1, I - 1, [I | L])
              end.