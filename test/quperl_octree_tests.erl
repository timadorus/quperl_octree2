%%
%% @doc test module quperl_octree2.
%%
%% @author Lutz Behnke <lutz.behnke@gmx.de>
%% @copyright 2015 Lutz Behnke
%%

%%
%% Tests TODO: append_node, first_node, rest_nodes, common_prefix
%%             filter_full_area, box_to_volume, xval, yval, zval, 
%%             bit_count, is_all_ones, is_all_zeroes, split/3, 

-module(quperl_octree_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("quperl_octree.hrl").

-import(quperl_octree,[new/0, new/1, new/2,

        normalize/2, to_node_id/2, to_node_list/1]).

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
                    ?_test(test_normalize_nodes(Args)),
                    ?_test(test_normalize_points(Args)),
                    ?_test(test_to_node_id(Args))
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


test_normalize_nodes(_Args) ->
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

test_normalize_points(_Args) ->

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


test_to_node_id(_Args) ->
        
    ?assertThrow({badarg, _Val}, to_node_id({0.0, 0.0, 1.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, to_node_id({0.0, 1.0, 0.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, to_node_id({1.0, 0.0, 0.0}, ?DEFAULT_MAX_DEPTH)),

    ?assertThrow({badarg, _Val}, to_node_id({0.0, 0.0, -1.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, to_node_id({0.0, -1.0, 0.0}, ?DEFAULT_MAX_DEPTH)),
    ?assertThrow({badarg, _Val}, to_node_id({-1.0, 0.0, 0.0}, ?DEFAULT_MAX_DEPTH)),

    ?assertMatch([7,0,0|_], to_node_list(to_node_id({0.5, 0.5, 0.5}, 
                                                    ?DEFAULT_MAX_DEPTH))),

    ok. 
