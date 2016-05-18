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

-import(quperl_octree_node_id,[to_node_id/1, to_node_id/2, to_node_list/1, get_value/2, get_depth/1]).

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
                   ]
      end }.

unit_test_() ->
    { "test each function directly",
      setup,
      fun setup_env/0,
      fun takedown_env/1,
      fun(_Args) -> [ ?_test(test_specific_points())
                    , ?_test(test_node_id_bit_count())
                    , ?_test(test_is_all_zeroes())
                    , ?_test(test_is_all_ones())
                    , ?_test(test_left_wall())
                    , ?_test(test_left_wall_at())
                    , ?_test(test_right_wall())
                    , ?_test(test_right_wall_at())
                    , ?_test(test_previous())
                    , ?_test(test_is_ancestor_of())
                    , ?_test(test_beyond())
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

                          InNodes = lists:map(fun(P1) -> to_node_id(P1) end,
                                              [[1],[2]]),

    Volume = quperl_octree:new_volume(InNodes),

?assertEqual(2, length(Volume#ot_volume.spaces)),

ok.


test_new_box_to_volume() ->

    TestVals = [
                 %% a single point should return a single point
                 {{0.499, 0.499, 0.499},
                  {0.499, 0.499, 0.499},
                  [to_node_list(to_node_id({0.499, 0.499, 0.499}))]}

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
                          Result = quperl_octree:new_box_to_volume(to_node_id(P1),
                                                                   to_node_id(P2)),
                          PrepResult = as_node_list(Result),
%%                           ?debugVal(PrepResult),
%%                           ?debugVal(length(Result)),
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
                          P1Id = to_node_id(P1),
                          P2Id = to_node_id(P2),

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
                          NodeId = to_node_id(Node),
                          P1Id = to_node_id(P1),
                          P2Id = to_node_id(P2),

                          Result = as_node_list(quperl_octree:handle_node_parent(I1, I2,
                                                                                 Dx, Dy, Dz,
                                                                                 NodeId,
                                                                                 P1Id, P2Id)),

                          ?assertEqual(Expected, Result)
                  end, TestVals),
    ok.

-dialyzer({no_match, set_value/3}).

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
                          Node = to_node_id(L),
                          Val = get_value(Node, z),
                          ND = get_depth(Node),

                          Result = quperl_octree:right_wall_calc(Val, ND, Depth),
                          ?assertEqual(Expected, to_node_list(set_value(Result, z, ND)))
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
                          Node = to_node_id(L),
                          Val = get_value(Node, z),
                          ND = get_depth(Node),

                          Result = quperl_octree:left_wall_calc(Val, Depth),
                          ?assertEqual(Expected, to_node_list(set_value(Result, z, ND)))
                  end, TestVals),

    ok.


test_beyond() ->
    TestVals = [{[1],[1,2],[2]}],

    lists:foreach(fun({A,P,R}) ->
                          ANode = to_node_id(A),
                          PNode = to_node_id(P),
                          Ret = to_node_list(quperl_octree:beyond(ANode, PNode)),
                          ?assertEqual(R, Ret)
                  end, TestVals),

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
                          ?assertEqual(R, quperl_octree:is_ancestor_of(to_node_id(A),
                                                       to_node_id(B)))
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
                          R1 = quperl_octree:previous(to_node_id(In)),
                          Result = to_node_list(R1),
                          ?assertEqual(Out, Result)
                  end, TestCases),

        ?assertThrow(badargs, quperl_octree:previous(#ot_node_id{depth=0})),
        ?assertThrow(badargs, quperl_octree:previous(to_node_id([0]))),
        ?assertThrow(badargs, quperl_octree:previous(to_node_id([0,0,0]))),

    ok.






test_is_all_zeroes() ->
    ?assertEqual(true, quperl_octree:is_all_zeroes(to_node_id([], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(true, quperl_octree:is_all_zeroes(to_node_id([0], ?DEFAULT_MAX_DEPTH))),

    ?assertEqual(false, quperl_octree:is_all_zeroes(to_node_id([4], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, quperl_octree:is_all_zeroes(to_node_id([2], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, quperl_octree:is_all_zeroes(to_node_id([1], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, quperl_octree:is_all_zeroes(to_node_id([7], ?DEFAULT_MAX_DEPTH))),

%%     ?assertError(function_clause, quperl_octree:is_all_zeroes(0)),
    ok.

test_is_all_ones() ->
    ?assertEqual(false, quperl_octree:is_all_ones(to_node_id([0], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, quperl_octree:is_all_ones(to_node_id([4], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, quperl_octree:is_all_ones(to_node_id([2], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(false, quperl_octree:is_all_ones(to_node_id([1], ?DEFAULT_MAX_DEPTH))),

    %% matter of definition: the required number of 0's for a path of 0 is 0!
    ?assertEqual(true, quperl_octree:is_all_ones(to_node_id([], ?DEFAULT_MAX_DEPTH))),

    ?assertEqual(false, quperl_octree:is_all_ones(to_node_id([0,7], ?DEFAULT_MAX_DEPTH))),

    ?assertEqual(true, quperl_octree:is_all_ones(to_node_id([7], ?DEFAULT_MAX_DEPTH))),
    ?assertEqual(true, quperl_octree:is_all_ones(to_node_id([7,7], ?DEFAULT_MAX_DEPTH))),

%%     ?assertError(function_clause, quperl_octree:is_all_ones(3)),
    ok.

test_node_id_bit_count() ->
    ?assertEqual(1, quperl_octree:bit_count(#ot_node_id{depth=1, x=1, y =0, z=0})),
    ?assertEqual(1, quperl_octree:bit_count(#ot_node_id{depth=1, x=0, y =1, z=0})),
    ?assertEqual(1, quperl_octree:bit_count(#ot_node_id{depth=1, x=0, y =0, z=1})),
    ?assertEqual(3, quperl_octree:bit_count(#ot_node_id{depth=1, x=1, y =1, z=1})),

    Val = (1 bsl 63) + 1,
    ?assertEqual(6, quperl_octree:bit_count(#ot_node_id{depth=64, x=Val, y=Val, z=Val})),

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




%%  set all the bits but the first to 1
test_right_wall() ->

    TestVals = [ {[],[],[],[]}
               , {[0], [0], [0], [0]}
               , {[0,0], [0,4], [0,2], [0,1]}
               , {[5,2,6], [1,6,6], [5,2,6], [4,3,7]}
               , {[7], [3], [5], [6]}
               ],

    lists:foreach(fun({I, XL, YL, ZL}) ->
                          N = to_node_id(I),

                          ?assertEqual(to_node_id(XL),
                                       quperl_octree:right_wall(N,x)),

                          ?assertEqual(to_node_id(YL),
                                       quperl_octree:right_wall(N,y)),

                          ?assertEqual(to_node_id(ZL),
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
                          N = to_node_id(I),

                          ?assertEqual(XL,
                                       to_node_list(quperl_octree:right_wall(N,D,x))),
                          ?assertEqual(YL,
                                       to_node_list(quperl_octree:right_wall(N,D,y))),
                          ?assertEqual(ZL,
                                       to_node_list(quperl_octree:right_wall(N,D,z)))

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
                          N = to_node_id(I),

                          ?assertEqual(to_node_id(XL),
                                       quperl_octree:left_wall(N,x)),

                          ?assertEqual(to_node_id(YL),
                                       quperl_octree:left_wall(N,y)),

                          ?assertEqual(to_node_id(ZL),
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
                          N = to_node_id(I),

                          ?assertEqual(XL,
                                       to_node_list(quperl_octree:left_wall(N,D,x))),

                          ?assertEqual(YL,
                                       to_node_list(quperl_octree:left_wall(N,D,y))),

                          ?assertEqual(ZL,
                                       to_node_list(quperl_octree:left_wall(N,D,z)))

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

-dialyzer({nowarn_function, as_node_list/1}).

%% @doc convert lists of lists of node_ids into node code lists
%% @private
-spec as_node_list( List :: NodeList )  ->
          [ CodeList ] when
 NodeList    :: NodeElement | [ NodeList ],
 NodeElement :: #ot_node_id{}
              | {#ot_node_id{}, #ot_node_id{}},
 CodeList    :: [ot_child_code()] | { [ot_child_code()], [ot_child_code()]} .

as_node_list([]) -> [];

as_node_list([First|Rest]) when is_list(First) ->
    [ as_node_list(First) | as_node_list(Rest) ];

as_node_list([P1|Rest]) when is_record(P1, ot_node_id) ->
   [ to_node_list(P1) | as_node_list(Rest)];

as_node_list([{P1, P2}|Rest]) ->
   [{to_node_list(P1), to_node_list(P2)} | as_node_list(Rest)].


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

