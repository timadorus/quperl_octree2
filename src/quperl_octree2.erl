%%
%% @doc manage arbitrary volumes within octree.
%% 
%% @author Lutz Behnke <lutz.behnke@gmx.de>
%%
%% @copyright Lutz Behnke 2015
%%


-module(quperl_octree2).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-record(octree, {}).

-type octree() :: #octree_rec.

-spec new() -> octree()

 quperl_octree2:new(Depth :: integer()) -> octree()

creates an empty volume, with depth 64, if none is given

 quperl_octree2:new(Point1 :: vec3d(), Point2 :: vec3d()) -> octree()
 
 where vec3d:  {X :: float(), Y :: float(), Z :: float()}
 
 create the minimal list of tree nodes required to fill axis aligned bounding box defined by minimal an maximal point.
 
 quperl_octree2:normalize_aabb({X1,Y1,Z1},{X1,Y1,Z1}) -> 
 
 normalizes two points to be used with new/2

%% ====================================================================
%% Internal functions
%% ====================================================================


