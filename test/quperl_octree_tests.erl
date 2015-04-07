%%
%% @doc test module quperl_octree2.
%%
%% @author Lutz Behnke <lutz.behnke@gmx.de>
%% @copyright 2015 Lutz Behnke
%%


-module(quperl_octree_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("quperl_octree.hrl").

-import(quperl_octree,[new/0, new/1, new/2,

        normalize/2]).

%%
%% Fixtures
%%


api_test_() ->
    { "provide minimal api tests",
      setup,
	  fun setup_env/0,
	  fun takedown_env/1,
      fun(Args) -> [
                    ?_test(test_new0(Args)),
                    ?_test(test_new1(Args)),
                    ?_test(test_normalize(Args))
                   ]
      end }.

setup_env() -> {args}.

takedown_env(_Args) -> ok.

test_new0(_Args) ->
    
    Octree = quperl_octree:new(),

    ?assertEqual(?DEFAULT_MAX_DEPTH, Octree#octree.max_depth),
    
    ok.

test_new1(_Args) -> 
    
    Octree = quperl_octree:new(10),

?assertEqual(10, Octree#octree.max_depth),

ok.


test_normalize(_Args) ->
    ?assertThrow(different_point_depth_not_supported, 
                 normalize(#ot_node_id{depth=1}, #ot_node_id{depth=2})),
    
    P1 = #ot_node_id{depth=62,x=3,y=2,z=5},
    P2 = #ot_node_id{depth=62,x=1,y=5,z=2},
    P1s = #ot_node_id{depth=62,x=1,y=2,z=2},
    P2s = #ot_node_id{depth=62,x=3,y=5,z=5},
    ?assertEqual({P1s, P2s}, normalize(P1, P2)),

    ok.

