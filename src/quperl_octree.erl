%%
%% @doc manage arbitrary volumes within octree.
%%
%% <p>volumes are encoded as sets of intervals of a Morton endoding. Individual 
%% start and end nodes may be ecnoded in one of two ways:</p>
%%
%% <ul>
%%  <li>as a record of depth and the path through the tree for each of the 
%%      dimensions, defined by an integer, holding the top most bit as the 
%%      most significant bit, for an integer of the same lenght as the max
%%      tree deth given.
%%  <li> a direct morton encoding as a list of integer values (0-7), 
%%      identifying the child node selected for traversal. The bits are 
%%      X,Y,Z axis from most to least significant bit. 
%%  <li> a direct morton encoding as integer value of 3*depth bit length, 
%%      identifying the child node selected for traversal. The bits for each 
%%      bit triplet are X,Y,Z axis from most to least significant bit. 
%% </ul>  
%% 
%% @author Lutz Behnke <lutz.behnke@gmx.de>
%%
%% @copyright Lutz Behnke 2015
%%


-module(quperl_octree).

-include_lib("eunit/include/eunit.hrl").
-include("quperl_octree.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0, new/1, new/2]).


-ifdef(TEST).
%% export the private functions for testing only.
-export([normalize/2]).
-endif.

%% new/0
%% --------------------------------------------------------------------
%% @doc create empty space for default max depth.
%% 
-spec new() -> octree().
%% --------------------------------------------------------------------
new() -> #octree{}.



%% new/1
%% --------------------------------------------------------------------
%% @doc create a octree record structure with a set depth.
%%
%% The function takes an integer value to create an empty structure of a given depth.
%%
-spec new(MaxDepth :: pos_integer()) -> octree().
%% --------------------------------------------------------------------
new(MaxDepth) when is_integer(MaxDepth) -> #octree{max_depth = MaxDepth}.

%% new/2
%% --------------------------------------------------------------------
%% @doc create an octree record structure from an axis aligned box.
%%
%% The function takes an axis aligned bounding box (AABB) to define a space 
%% to occupy part of the volume encompassed by the complete octree.
%%
-spec new(Point1 :: vec_3d(), Point2 :: vec_3d()) -> octree().
%% --------------------------------------------------------------------
new(Point1, Point2) ->
    {MinPoint, MaxPoint} = normalize(Point1, Point2).

%% creates an empty volume, with depth 64, if none is given
%% 
%%  quperl_octree2:new(Point1 :: vec3d(), Point2 :: vec3d()) -> octree()
%%  
%%  where vec3d:  {X :: float(), Y :: float(), Z :: float()}
%%  
%%  create the minimal list of tree nodes required to fill axis aligned bounding box defined by minimal an maximal point.
%%  
%%  quperl_octree2:normalize_aabb({X1,Y1,Z1},{X1,Y1,Z1}) -> 
%%  

%% ====================================================================
%% Internal functions
%% ====================================================================


%% normalize/2
%% --------------------------------------------------------------------
%% @doc normalize two points so that point 1 will hold the lesser, point 2
%%      the greater value in all dimensions.
%% @end
-spec normalize(P1 :: #ot_node_id{}, P2 :: #ot_node_id{}) -> 
          {#ot_node_id{}, #ot_node_id{}}.
%% --------------------------------------------------------------------

normalize(#ot_node_id{depth=D1, x=_, y=_, z=_},
          #ot_node_id{depth=D2, x=_, y=_, z=_}) when D1 =/= D2 ->
    throw(different_point_depth_not_supported);

normalize(#ot_node_id{depth=D, x=X1, y=Y1, z=Z1},
          #ot_node_id{depth=D, x=X2, y=Y2, z=Z2}) ->
    {#ot_node_id{depth=D, x= min(X1,X2), y= min(Y1,Y2), z=min(Z1,Z2)},
     #ot_node_id{depth=D, x= max(X1,X2), y= max(Y1,Y2), z=max(Z1,Z2)}}.
    
