-module(performance_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-export [new_volume_param_wrap/1, to_pos_code_param_wrap/1].


suite() -> [].
groups() -> [{test_octree, [sequence, {repeat, 2}], 
              [many_points, many_volumes]}].
all() -> [{group, test_octree}].
    
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) -> ok.

init_per_group(_GroupName, Config) ->
    Config.
    
end_per_group(_GroupName, Config) ->
    Config.

%%
%% init and end per Testcase
%%
init_per_testcase(_Other, Config) ->
    Config.


end_per_testcase(_Other, _Config) ->
    ok.

%%
%% test functions named in groups....
%%

many_points(_Config) ->
    teu_perf_measure:test_avg_func("point to pos_code", 
                                   ?MODULE, to_pos_code_param_wrap, [], 
                                   [], fun gen_point/1, 10000),
    ok.


to_pos_code_param_wrap(Pos) ->
      quperl_octree_pos_code:to_pos_code(quperl_octree:to_node_id(Pos)).


many_volumes(_Config) ->
    teu_perf_measure:test_avg_func("volume to node list", 
                                   ?MODULE, new_volume_param_wrap, [], 
                                   [], fun gen_volume/1, 10),
    ok.


%% TODO: convert all nodes in volume 
new_volume_param_wrap({P1, P2}) ->
      quperl_octree:new_volume(P1, P2).


-define(AREA_SIZE, 0.00001).

%% gen_volume/1
%% @doc generate an volume within the norm cube.
%% the volume defined by two points spanning a axis aligned bounding box.
%% @end
gen_volume(_OldArea) ->
    AreaSize = math:pow(10, -1 * (2 + random:uniform(10))),
    %% AreaSize = ?AREA_SIZE,
    X = random:uniform() * (1.0 - AreaSize),
    Y = random:uniform() * (1.0 - AreaSize),
    Z = random:uniform() * (1.0 - AreaSize),
    {{X,Y,Z}, {X + AreaSize, Y + AreaSize, Z + AreaSize}}.


%% gen_point/1
%% @doc generate an area of the norm square.
%% the area is defined by a point and a size
%% @end
gen_point(_OldPoint) ->
    X = random:uniform(),
    Y = random:uniform(),
    Z = random:uniform(),
    {X,Y,Z}.

%%
%%   Utility Functions
%%

