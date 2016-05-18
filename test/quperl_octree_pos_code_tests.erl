%% @author sage
%% @doc tests for module quperl_octree_pos_code


-module(quperl_octree_pos_code_tests).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("quperl_octree.hrl").

-import(quperl_octree_pos_code, [is_pos_code/1, to_pos_code/1]).

-define(MAX_SIZE,1.0 - 1.0e-15).
%% --------------------------------------------------------------------
%% Fixtures
%% --------------------------------------------------------------------

%% info_test_() ->
%%     { setup, fun() -> ok end,
%%       fun() -> ?debugFmt("~n############################################~n      starting ~p~n############################################~n  ", [?MODULE]) end }.

local_test_() ->
    { "test functions locally and test internal functions",
      setup,

      fun() ->
              ok
      end,

      fun(_Args) ->
              ok
      end,
      fun(_Foo) -> [ ?_test(test_new_pos_code())
                   , ?_test(test_is_pos_code())
                   ]
      end }.

%% ====================================================================
%% Internal functions
%% ====================================================================

test_is_pos_code() ->
    lists:foreach(fun do_is_pos_code_check/1, test_cases()),
    ok.

do_is_pos_code_check({ok, _P, _B, _ExpectedAC, PC, _Parent}) ->
%%     ?debugFmt("expect true for ~p",[PC]),
    ?assertEqual(true, is_pos_code(PC));

do_is_pos_code_check({_S, _P, _B, _ExpectedAC, PC, _Parent}) ->
%%     ?debugFmt("expect false for ~p",[PC]),
    ?assertEqual(false, is_pos_code(PC)).



%% test_new_pos_code/0
%% --------------------------------------------------------------------
%% @doc verify that the octree paths are correctly packed.
%% @end
%% --------------------------------------------------------------------
-spec test_new_pos_code() -> ok.
test_new_pos_code() ->
%%     ?debugMsg("starting make_pos_test"),
    lists:foreach(fun do_test_case/1, test_cases()),
    ok.

do_test_case({ok, Pos1 = {P1, P2, P3}, {B1, B2, B3}, _ExpectedAC, ExpectedPC, _Parent}) ->
    Pos2 = {P1 + B1, P2 + B2, P3 + B3},

    Node = min_super_node(Pos1, Pos2, ?DEFAULT_MAX_DEPTH),
    Ret = to_pos_code(Node),

%%     ?debugFmt("~n position: ~p~n expected:  ~P~n generated: ~P~n",
%%               [Pos1, ExpectedPC, 100, Ret, 100]),

    ?assertEqual( ( 8
                  + (?DEFAULT_MAX_DEPTH*3)
                  + ((8- ((?DEFAULT_MAX_DEPTH*3) rem 8)) rem 8)
                  ),
                 bit_size(Ret)),
    ?assertEqual(ExpectedPC, Ret),

    ok;

do_test_case(_) -> ok.

%% ====================================================================
%% utility functions for tests
%% ====================================================================

test_cases() -> [{ok,
                  {0.585702840753962, 0.8471973103830974, 0.7459485676749666},
                  {1.0e-6,1.0e-6,1.0e-6},
                  [7,2,1,7,3,5,1,4,7,7,7,5,0,1,1,2], % depth = 16
                  <<16:8,7:3,2:3,1:3,7:3,3:3,5:3,1:3,4:3,7:3,7:3,7:3,5:3,0:3,1:3,1:3,2:3,0:((44*3)+4)>>,
                  <<15:8,7:3,2:3,1:3,7:3,3:3,5:3,1:3,4:3,7:3,7:3,7:3,5:3,0:3,1:3,1:3,0:((43*3)+4)>>},

                 {ok,
                  {0.000001, 0.000001, 0.000001},
                  {0.9999, 0.9999, 0.9999},
                  [],
                  <<0:192>>,
                  <<0:192>>},

                 {{error, badarg},
                  {1.1, 1.1, 1.1},
                  {0.1, 0.1, 0.1},
                  [],
                  <<0>>,
                  <<0>>},

                 {{error, badarg},
                  {0.1, 1.1, 1.1},
                  {0.1, 0.1, 0.1},
                  [],
                  <<0>>,
                  <<0>>},

                 {{error, badarg},
                  {0.1, 0.1, 1.1},
                  {0.1, 0.1, 0.1},
                  [],
                  <<0>>,
                  <<0>>},

                 {{error, badarg},
                  {0.1, 0.1, 0.1},
                  {1.0, 1.0, 1.0},
                  [],
                  <<0>>,
                  <<0>>},

                 {ok,
                  {0.1, 0.1, 0.1},
                  {0.16, 0.16, 0.16},
                  [0],
                  <<1:8,0:184>>,
                  <<0:8,0:184>>},

                 {ok,
                  {0.5, 0.5, 0.5},
                  {0.26, 0.26, 0.26},
                  [7],
                  <<1:8,7:3,0:((59*3)+4)>>,
                  <<0:8,0:((58*3)+4)>>},

                 {ok,
                  {0.1, 0.1, 0.1},
                  {0.01, 0.01, 0.01},
                  [0,0,0,7,7],
                  <<5:8,0:3,0:3,0:3,7:3,7:3,0:((55*3)+4)>>,
                  <<4:8,0:3,0:3,0:3,7:3,0:(56*3)>>},

                 {ok,
                  {0.1, 0.2, 0.3},
                  {0.01, 0.01, 0.01},
                  [0,1,2,6,5],
                  <<5:8,0:3,1:3,2:3,6:3,5:3,0:((55*3)+4)>>,
                  <<4:8,0:3,1:3,2:3,6:3,0:(56*3)>>},

                 {ok,
                  {0.01, 0.01, 0.01},
                  {0.5, 0.5, 0.5},
                  [],
                  <<0:192>>,
                  <<0:192>>},

                 {ok,
                  {1.0e-7, 1.0e-7, 1.0e-7},
                  {1.0e-6, 1.0e-6, 1.0e-6},
                  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],  % depth = 19
                  <<19:8,0:184>>,
                  <<18:8,0:184>>},

                 {ok,
                  {0.01, 0.01, 0.01},
                  {0.1, 0.2, 0.3},
                  [0],
                  <<1:8,0:184>>,
                  <<0:8,0:184>>},

                 {{error, badarg},
                  {0.0, 0.0, 0.0},
                  {1.0, 1.0, 1.0},
                  [],
                  <<0>>,
                  <<0>>},

                 {{error, badarg},
                  {0.0, 0.0, 0.0},
                  {0.0, 1.0, 1.0},
                  [],
                  <<0>>,
                  <<0>>},

                 {{error, badarg},
                  {0.0, 0.0, 0.0},
                  {0.0, 0.0, 1.0},
                  [],
                  <<0>>,
                  <<0>>},

                 {ok,
                  {0.01, 0.01, 0.01},
                  {0.0, 0.0, 0.0},
                  [0,0,0,0,0,0,7,0,7,0,0,0,7,7,7,7,0,7,0,7,7,7,0,0,0,0,7,0,7,0,0,0,
                   7,7,7,7,0,7,0,7,7,7,0,0,0,0,7,0,7,0,0,0,7,7,7,7,0,7,7,0,0,0,0,0],
                  <<60,0,0,56,224,15,255,28,127,192,3,142,0,255,241,199,252,0,56,224,
              15,255,31,128>>,
                  <<59,0,0,56,224,15,255,28,127,192,3,142,0,255,241,199,252,0,56,224,
              15,255,31,128>>},


                 {ok,
                  {0.5, 0.5, 0.5},
                  {0.0, 0.0, 0.0},
                  [],
                  <<60,224,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
                  <<59,224,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>},


                {{error, no_pos_code},
                 {invalid, invalid, invalid},
                 {invalid, invalid, invalid},
                 [],
                 <<1,2,3>>,
                 <<1,2,3>>},

                {{error, no_pos_code},
                 {invalid, invalid, invalid},
                 {invalid, invalid, invalid},
                 [],
                 <<61,2,3>>,
                 <<60,2,3>>},

                {{error, no_pos_code},
                 {invalid, invalid, invalid},
                 {invalid, invalid, invalid},
                 [],
                 <<61,224,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
                 <<60,224,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>}
                ].

min_super_node(P1, P2, Depth) ->
    {Pre, _R1, _R2} = quperl_octree_node_id:common_prefix(quperl_octree_node_id:to_node_id(P1, Depth),
                                                  quperl_octree_node_id:to_node_id(P2, Depth)),
    Pre.