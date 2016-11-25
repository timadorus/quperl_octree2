%% @author sage
%% @doc @todo Add description to quperl_fast_box_to_volume.


-module(quperl_fast_box_to_volume_tests).

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

      fun() ->
              ok
      end,

      fun(_Args) ->
              ok
      end,
      fun(_Args) -> [ ?_test(test_wall_align())
                    ]
      end
    }.


%% ====================================================================
%% Internal functions
%% ====================================================================

test_wall_align() ->
  Node = quperl_octree_node_id:to_node_id([1]),
  Point = quperl_octree_node_id:to_node_id([2]),

  ?assertEqual(Node, quperl_fast_box_to_volume:wall_align(Node, Point)),

  ok.
