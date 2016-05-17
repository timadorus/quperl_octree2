%%
%% @doc manage arbitrary volumes within octree.
%%
%% the space encompassed by the octree forms a normcube with a length of 1 in
%% each dimension. The dimensions x,y,z form a right handed cartesian
%% coordination system with the origin at [0,0,0].
%%
%% <p>volumes are encoded as sets of intervals of a Morton endoding. Individual
%% start and end nodes may be encoded in one of two ways:</p>
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
%% @copyright Lutz Behnke 2015-2016
%%


-module(quperl_octree).

-include_lib("eunit/include/eunit.hrl").
-include("quperl_octree.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-opaque node_id() :: ot_node_id().
-opaque volume() :: ot_volume().

-export_type([node_id/0, volume/0]).

-export([new_volume/0, new_volume/1, new_volume/2,
         to_node_id/1, to_node_id/2,
         to_node_list/1, inner/1, leaf/1,
         is_equal/2, get_depth/1, get_value/2]).


-ifdef(TEST).
%% export the private functions for testing only.
-export([normalize/2, append_node/2, first_node/1,
         common_prefix/2, previous/1,
         is_all_ones/1, is_all_zeroes/1, xval/1, yval/1, zval/1,
         rest_nodes/1, bit_count/1, default_max_depth/0,
         for_each_child/3, is_ancestor_of/2, beyond/2, get_code_at/2,
         left_wall/2, right_wall/2, left_wall/3, right_wall/3,
         handle_node_parent/8, make_sub_node_points/8,
         left_wall_calc/2, right_wall_calc/3, prepend_node/2,
         new_box_to_volume/2
        ]).
-endif.

%% to_node_id/3,
%%          box_to_volume/1, box_to_volume/2, filter_full_area/1,
%%
%%
%%          split/4,
%%          sweep/1,
%%

%% new/0
%% --------------------------------------------------------------------
%% @doc create empty volume for default max depth.
%%
-spec new_volume() -> ot_volume().
%% --------------------------------------------------------------------
new_volume() -> #ot_volume{}.



%% new_volume/1
%% --------------------------------------------------------------------
%% @doc create a octree volume structure from a list of node Ids.
%%
%%
-spec new_volume(NodeList :: [ot_node_id()]) -> ot_volume().
%% --------------------------------------------------------------------
new_volume(NodeList) when is_list(NodeList) ->
    #ot_volume{max_depth = ?DEFAULT_MAX_DEPTH, spaces=NodeList}.


%% new_volume/2
%% --------------------------------------------------------------------
%% @doc create an octree volume from an axis aligned box.
%%
%% The function takes an axis aligned bounding box (AABB) specified by
%% two points. Returns a ot_volume() that contains the minimal set of
%% octree nodes need to fully encompas the volume.
%%
-spec new_volume(Point1 :: vec_3d() | ot_node_id(),
                 Point2 :: vec_3d() | ot_node_id()) ->
          ot_volume().
%% --------------------------------------------------------------------
new_volume(Point1, Point2)when is_record(Point1, ot_node_id)
                           and is_record(Point2, ot_node_id) ->
    {P1, P2} = normalize(Point1,Point2),
    new_volume(new_box_to_volume(P1,P2));

new_volume(P1 = {_X1, _Y1, _Z1}, P2 = {_X2, _Y2, _Z2}) ->
    new_volume(to_node_id(P1, ?DEFAULT_MAX_DEPTH),
               to_node_id(P2, ?DEFAULT_MAX_DEPTH)).


%% to_node_id/1
%% --------------------------------------------------------------------
%% @doc create a node identifier from a 3D Vector, using default depth.
%%
%% The function will return the octree leaf node that contains the point
%% given by the argument.
%% @end
-spec to_node_id(vec_3d() | ot_node_list()) -> ot_node_id().
%% --------------------------------------------------------------------
to_node_id(Val) -> to_node_id(Val, ?DEFAULT_MAX_DEPTH).

%% to_node_id/2
%% --------------------------------------------------------------------
%% @doc create a node identifier from a 3D Vector.
%%
%% The function will return the octree leaf node that contains the point
%% given by the argument.
%% @end
-spec to_node_id(vec_3d() | ot_node_list(),
                 MaxDepth :: non_neg_integer()) -> ot_node_id().
%% --------------------------------------------------------------------
to_node_id(Val = {V,_,_}, _D) when (V < 0.0) or (V >= 1.0) ->
    throw({badarg, Val});

to_node_id(Val = {_,V,_}, _D) when (V < 0.0) or (V >= 1.0) ->
    throw({badarg, Val});

to_node_id(Val = {_,_,V}, _D) when (V < 0.0) or (V >= 1.0) ->
    throw({badarg, Val});

to_node_id({X,Y,Z}, Depth) ->
    Xl = trunc(X*(1 bsl Depth)),
    Yl = trunc(Y*(1 bsl Depth)),
    Zl = trunc(Z*(1 bsl Depth)),

    #ot_node_id{depth = Depth, x=Xl, y=Yl, z=Zl};

to_node_id(NodeList, Depth) when is_list(NodeList) ->
    to_node_id(NodeList, #ot_node_id{}, Depth).




%% leaf/1
%% --------------------------------------------------------------------
%% @doc return the leaf element of a pos code.
%%
%% The function will return the node identifier of the lowest, thus smallest node,
%% that forms the end of the node path.
%%
%% It will throw <tt>zero_depth</tt> if the node path is of length 0.
%%
%% @end
-spec leaf(Point :: ot_node_id()) -> ot_child_code() | no_return().
%% --------------------------------------------------------------------
leaf(#ot_node_id{depth=0}) -> throw(zero_depth);

leaf(Point) when is_record(Point, ot_node_id) ->
    Pos = ?DEFAULT_MAX_DEPTH - Point#ot_node_id.depth,
    Mask = (1 bsl Pos),

    (((Point#ot_node_id.x band Mask) bsr Pos) * ?XMult +
     ((Point#ot_node_id.y band Mask) bsr Pos) * ?YMult +
     ((Point#ot_node_id.z band Mask) bsr Pos) * ?ZMult).


%% inner/1
%% --------------------------------------------------------------------
%% @doc get path of inner nodes.
%%
%% Function will return all path elements except the last (i.e the leaf node)
%%
%% @end
%% --------------------------------------------------------------------
-spec inner(Point :: #ot_node_id{}) -> #ot_node_id{} | no_return().
inner(#ot_node_id{depth=0}) -> throw(zero_depth);

inner(#ot_node_id{depth=Depth, x=X, y=Y, z=Z}) ->
    Pos = (?DEFAULT_MAX_DEPTH - Depth),
    Mask = (?ALL_BITS_MASK bxor (1 bsl Pos)),

    to_node_id(Depth-1, X band Mask, Y band Mask, Z band Mask).


%% is_equal/2
%% --------------------------------------------------------------------
%% @doc return true if two points are the same, false otherwise.
%%
%% Two points are considered the same, if their depth is equal and
%% the path through the tree is the same.
%%
%% @end
-spec is_equal(P1 :: #ot_node_id{}, P2 :: #ot_node_id{}) -> true|false.
%% --------------------------------------------------------------------
is_equal(#ot_node_id{depth = D, x=X, y=Y, z=Z},
         #ot_node_id{depth = D, x=X, y=Y, z=Z}) -> true;

is_equal(_,_) -> false.


%% get_depth/1
%% --------------------------------------------------------------------
%% @doc get the depth a node
%% @end
-spec get_depth(Node :: #ot_node_id{}) -> non_neg_integer().
%% --------------------------------------------------------------------
get_depth(Node) -> Node#ot_node_id.depth.


%% get_value/2
%% --------------------------------------------------------------------
%% @doc get the point value per dimension
%% @end
%% --------------------------------------------------------------------
-spec get_value(Node :: #ot_node_id{}, Dim :: x|y|z) -> non_neg_integer().
get_value(Node, x) -> Node#ot_node_id.x;
get_value(Node, y) -> Node#ot_node_id.y;
get_value(Node, z) -> Node#ot_node_id.z.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% to_node_id/4
%% --------------------------------------------------------------------
%% @doc create ot_node_id structure from components.
%% @end
-spec to_node_id(Depth :: non_neg_integer(), X :: non_neg_integer(),
                  Y :: non_neg_integer(), Z :: non_neg_integer()) ->
           #ot_node_id{}.
%% --------------------------------------------------------------------
to_node_id(Depth, X, Y, Z) -> #ot_node_id{depth=Depth, x=X, y=Y, z=Z}.


-spec get_code_at(Depth :: non_neg_integer(), Node :: #ot_node_id{}) -> ot_child_code().
get_code_at(Depth, Node) ->
    Shift = (?DEFAULT_MAX_DEPTH - Depth),
    Mask = (1 bsl Shift),

    (((Node#ot_node_id.x band Mask) bsr Shift) * ?XMult +
     ((Node#ot_node_id.y band Mask) bsr Shift) * ?YMult +
     ((Node#ot_node_id.z band Mask) bsr Shift) * ?ZMult).


%% for_each_child/3
%% --------------------------------------------------------------------
%% @doc execute function on each child of a node.
%% @end
%% --------------------------------------------------------------------
-spec for_each_child(N :: #ot_node_id{},
                     F :: fun((Child :: #ot_node_id{}, ExtraIn :: term()) -> term()),
                     Extra :: term()) -> [term()].
for_each_child(Node, Fun, Extra) ->
    lists:foldl(fun(L, Acc) -> Acc ++ Fun(append_node(Node,L), Extra) end, [], lists:seq(0, 7)).


%% is_ancestor_of/2
%% --------------------------------------------------------------------
%% @doc return if N1 is parent of N2, false otherwise.
%% @end
%% --------------------------------------------------------------------
-spec is_ancestor_of(N1 :: #ot_node_id{}, N2 :: #ot_node_id{}) -> boolean().

is_ancestor_of(P1, P2) when P1#ot_node_id.depth >= P2#ot_node_id.depth ->
    false;

is_ancestor_of(P1, P2)  ->
    D = P1#ot_node_id.depth,

    is_ancestor_of(P1#ot_node_id.x, P2#ot_node_id.x, D)
        and is_ancestor_of(P1#ot_node_id.y, P2#ot_node_id.y, D)
        and is_ancestor_of(P1#ot_node_id.z, P2#ot_node_id.z, D).

is_ancestor_of(N1, N2, D) ->
    Mask = bnot (?ALL_BITS_MASK bsr D),
    N1 == (N2 band Mask).


%% new_box_to_volume/2
%% --------------------------------------------------------------------
%% @doc convert an axis aligned bounding box to a list of node ids.
%% @end
%% --------------------------------------------------------------------
-spec new_box_to_volume(P1 :: ot_node_id(), P2 :: ot_node_id()) -> [ot_node_id()].
new_box_to_volume(P1, P2) when P1#ot_node_id.depth == P2#ot_node_id.depth ->
    new_box_to_volume(#ot_node_id{}, P1, P2).


%% new_box_to_volume/3
%% --------------------------------------------------------------------
%% @doc find nodes that are part of volume in sub-tree that has node as
%%  root.
%% @end
%% --------------------------------------------------------------------
-spec new_box_to_volume(N :: ot_node_id(), P1 :: ot_node_id(), P2 :: ot_node_id()) -> [ot_node_id()].
new_box_to_volume(_P, Point, Point) -> [Point];

new_box_to_volume(Parent, Point1, Point2) ->
    Beyond1 = beyond(Parent, Point1),
    Beyond2 = beyond(Parent, Point2),

    case is_all_zeroes(Beyond1) and is_all_ones(Beyond2) of
        %% the points encompass all of the node..
        true ->
            [Parent];
        %% the actual area is within the node
        false ->
            for_each_child(Parent, fun box_to_vol_per_node/2, {Point1, Point2})
end.

box_to_vol_per_node(Node, {Point1, Point2}) ->
                      Parent1 = is_ancestor_of(Node, Point1),
                      Parent2 = is_ancestor_of(Node, Point2),

                      V1 = get_code_at(get_depth(Node), Point1),
                      V2 = get_code_at(get_depth(Node), Point2),

                      Dxyz = V1 bxor V2,
                      Dx = xval(Dxyz), Dy = yval(Dxyz), Dz = zval(Dxyz),

                      handle_node_parent(Parent1, Parent2, Dx,Dy,Dz, Node, Point1, Point2).

%% handle_node_parent/8
%% --------------------------------------------------------------------
%% @doc compute the resultant nodes for point and node configuration
%%
%% The axis deltas (Dx,Dy,Dz) indicate whether the other point is
%% in the upper node (true) or not (false). Since P1 and P2 are
%% normalized, the line allways passes from P1 to the node border to
%% P2. Thus a false will indicate a box from the border to the P2
%% value for that dimension.
%%
%%
%% The following cases are possible:
%% <dl>
%%  <dt><tt>{f,f,f,f,f}</tt></dt>
%%      <dd>ignore. Neither point lies in node, nor does any overlap this
%%          node.</dd>
%%  <dt><tt>{f,f,_,_,_}</tt></dt>
%%      <dd>create two new points, based on border passes, descend.</dd>
%%  <dt><tt>{t,t,f,f,f}</tt></dt>
%%      <dd>both points lie in node. If the points cover the two mos extreme
%%          points of the node (all zero and all one respectively), this node
%%          is fully covered and must be returned as leaf. Descend with both
%%          points otherwise.</dd>
%%  <dt><tt>{t,t,_,_,_}</tt></dt>
%%      <dd>invalid</dd>
%%  <dt><tt>{t,f,X,Y,Z}</tt> when (X or Y or Z) == false</dt>
%%      <dd>volume has less than three dims, thus empty sub tree.</dd>
%%  <dt><tt>{f,t,X,Y,Z}</tt> when (X or Y or Z) == false</dt>
%%      <dd>volume has less than three dims, thus empty sub tree.</dd>
%%  <dt><tt>{t,f,_,_,_}</tt></dt>
%%      <dd>for each (D == true): split at upper border; descend with (P1, NewPoint)</dd>
%%  <dt><tt>{f,t,_,_,_}</tt></dt>
%%      <dd>for each (D == true): split at lower border; descend with (NewPoint, P2)</dd>
%% </dl>
%%
%% For reasons of readability, the invalid cases are not filtered out.
%% @private
%% @end
%% --------------------------------------------------------------------
-spec handle_node_parent(P1InNode, P2InNode, Dx,Dy,Dz, Node, P1, P2) -> [#ot_node_id{}] when
          P1InNode  :: boolean(),
    P2InNode  :: boolean(),
    Dx  :: 0|1,
    Dy  :: 0|1,
    Dz  :: 0|1,
    Node :: #ot_node_id{},
    P1 :: #ot_node_id{},
    P2 :: #ot_node_id{}.

%% both points in the node....
handle_node_parent(true, true, _Dx, _Dy, _Dz, Node, Point1, Point2) ->
    new_box_to_volume(Node, Point1, Point2);

%% volume is lacking delta in at least one dimension -> will not create volume, but point, line or area
handle_node_parent(false, false, Dx, Dy, Dz, _N, _P1, _P2) when (Dx + Dy + Dz) < 3 -> [];
handle_node_parent(true, false, Dx, Dy, Dz, _N, _P1, _P2) when (Dx + Dy + Dz) < 3 -> [];
handle_node_parent(false, true, Dx, Dy, Dz, _N, _P1, _P2) when (Dx + Dy + Dz) < 3 -> [];



%% if both points are outside the node, the box may still overlap
%% N1x = if(Dx) right_wall(Node, x), else P2x
%% N2x = If(Dx) P1x, else left_wall(Node, x)

handle_node_parent(In1, In2,
                   Dx, Dy, Dz,
                   Node = #ot_node_id{depth =D}, Point1, Point2) ->

    {NX1, NX2} = make_sub_node_points(In1, In2,
                                      Dx,
                                      is_upper(get_code_at(D, Node), x),
                                      D, x,
                                      Point1, Point2),
    {NY1, NY2} = make_sub_node_points(In1, In2,
                                      Dy,
                                      is_upper(get_code_at(D, Node), y),
                                      D, y,
                                      Point1, Point2),
    {NZ1, NZ2} = make_sub_node_points(In1, In2,
                                      Dz,
                                      is_upper(get_code_at(D, Node), z),
                                      D, z,
                                      Point1, Point2),

    NewPoint1 = #ot_node_id{depth = Point1#ot_node_id.depth,
                            x =NX1, y =NY1, z = NZ1
                           },
    NewPoint2 = #ot_node_id{depth = Point2#ot_node_id.depth,
                            x =NX2, y =NY2, z = NZ2
                           },

    new_box_to_volume(Node, NewPoint1, NewPoint2).


%% determine if node is on the upper or lower part of the qube
-spec is_upper(Pos :: ot_child_code(), Dim :: x|y|z) -> boolean().
is_upper(Pos, x) -> xval(Pos) == 1;
is_upper(Pos, y) -> yval(Pos) == 1;
is_upper(Pos, z) -> zval(Pos) == 1.

%% make_sub_node_points/7
%% --------------------------------------------------------------------
%% @doc create two points from two points that may be outside of current node.
%% @end
-spec make_sub_node_points(IP1 :: boolean(), IP2 :: boolean(),
                           Delta :: 0|1, Upper ::boolean(),
                           Depth :: non_neg_integer(),
                           Dim :: x|y|z,
                           P1 :: #ot_node_id{}, P2 :: #ot_node_id{}) ->
          {non_neg_integer(), non_neg_integer()}.
%% --------------------------------------------------------------------
make_sub_node_points(false, false, 0, _Upper, _Depth, _Dim, _P1, _P2) ->
    %% should not call this function, but be handled above
    throw({badargs, false, false, 0});

%% first point is in, second is, out, there is a differences, but this not the upper.
make_sub_node_points(true, false, 0, false, _Depth, Dim, P1, P2) ->
      P1Val = get_value(P1, Dim),
      P2Val = get_value(P2, Dim),
      {P1Val, P2Val};

make_sub_node_points(true, false, 1, _Upper, Depth, Dim, P1, _P2) ->
      P1Val = get_value(P1, Dim),
      ND = get_depth(P1),
      {P1Val, right_wall_calc(P1Val, ND, Depth)};

make_sub_node_points(false, true, 0, _Upper, _Depth, Dim, P1, P2) ->
      P1Val = get_value(P1, Dim),
      P2Val = get_value(P2, Dim),
      {P1Val, P2Val};

make_sub_node_points(false, true, 1, _Upper, Depth, Dim, _P1, P2) ->
      P2Val = get_value(P2, Dim),
      {left_wall_calc(P2Val,Depth), P2Val};


make_sub_node_points(false, false, 1, false, Depth, Dim, P1, _P2) ->
      P1Val = get_value(P1, Dim),
      ND = get_depth(P1),
      {P1Val, right_wall_calc(P1Val, ND, Depth)};

make_sub_node_points(false, false, 1, true, Depth, Dim, _P1, P2) ->
      P2Val = get_value(P2, Dim),
      {left_wall_calc(P2Val,Depth), P2Val};

make_sub_node_points(In1, In2, Delta, Upper, Depth, Dim, P1, P2) ->
    %% should not call this function, but be handled above
    throw({badargs, unkown_combination, {In1, In2, Delta, Upper, Depth, Dim, to_node_list(P1), to_node_list(P2)}}).



%% beyond/2
%% --------------------------------------------------------------------
%% @doc construct a node from a tree path beyond an ancestor.
%%
%% This basically a tail function on the node list.
%%
%% Example: beyond([1,2],[1,2,3,4]) -> [3,4]
%%
%% it does the same as calling rest_nodes/1 as many times as the
%% depth of the ancestor, but is more efficient.
%%
%% calling beyond with an ancestor of less depth than the point
%% will throw badargs
%% @end
%% --------------------------------------------------------------------
-spec beyond(Ancestor :: #ot_node_id{}, Point :: #ot_node_id{} ) -> #ot_node_id{}.

beyond(A, P) when A#ot_node_id.depth >= P#ot_node_id.depth ->
    throw(badargs);

beyond(A, #ot_node_id{depth=PD, x=X, y=Y, z=Z}) ->
    AD = A#ot_node_id.depth,
    Mask = ?ALL_BITS_MASK bsr AD,
    NewX = ((X band Mask) bsl AD),
    NewY = ((Y band Mask) bsl AD),
    NewZ = ((Z band Mask) bsl AD),
    #ot_node_id{depth=PD-AD, x=NewX , y= NewY, z= NewZ}.



-define(LEFT_WALL, 1 bsl (?DEFAULT_MAX_DEPTH - 1)).

%% left_wall/2
%% --------------------------------------------------------------------
%% @doc set all bits but the first of given dimension to 0, thus following the left
%%      wall of the octree area.
%%
%% <p>Note: the functions is only correct for node depth > 0</p>
%% @end
-spec left_wall(#ot_node_id{}, x|y|z) -> #ot_node_id{}.
%% --------------------------------------------------------------------
left_wall(Point, Dim) ->
    left_wall(Point, 1, Dim).


%% left_wall/3
%% --------------------------------------------------------------------
%% @doc set all bits but the first of given dimension to 0, thus following the left
%%      wall of the octree area. Start this at depth D ( > 0)
%%
%% <p>Note: the functions is only correct for node depth > 0</p>
%% <p>Precondition: D must not be greater than node depth
%%
%% @end
-spec left_wall(#ot_node_id{}, Depth :: non_neg_integer(), x|y|z) -> #ot_node_id{}.
%% --------------------------------------------------------------------
left_wall(Point, Depth, x) ->
    Point#ot_node_id{x = left_wall_calc(Point#ot_node_id.x, Depth)};

left_wall(Point, Depth, y) ->
    Point#ot_node_id{y = left_wall_calc(Point#ot_node_id.y, Depth)};

left_wall(Point, Depth, z) ->
    Point#ot_node_id{z = left_wall_calc(Point#ot_node_id.z, Depth)}.


left_wall_calc(In, Depth) ->
    Mask = ?ALL_BITS_MASK bxor (?ALL_BITS_MASK bsr (Depth-1)),
    (In band Mask) bor ( 1 bsl (?DEFAULT_MAX_DEPTH - (Depth))).


%% right_wall/2
%% --------------------------------------------------------------------
%% @doc set all bits but the first of given dimension to one, thus following the right
%%      wall of the octree area.
%% @end
-spec right_wall(#ot_node_id{}, x|y|z) -> #ot_node_id{}.
%% --------------------------------------------------------------------
right_wall(Point, Dim) ->
    right_wall(Point, 1, Dim).


%% right_wall/3
%% --------------------------------------------------------------------
%% @doc set all bits but the first of given dimension to one, thus following the right
%%      wall of the octree area.
%% @end
-spec right_wall(#ot_node_id{}, Depth :: non_neg_integer(), x|y|z) -> #ot_node_id{}.
%% --------------------------------------------------------------------
right_wall(Point = #ot_node_id{depth = ND, x = X}, Depth, x) ->
    Point#ot_node_id{x = right_wall_calc(X, ND, Depth)};

right_wall(Point = #ot_node_id{depth = ND, y = Y}, Depth, y) ->
    Point#ot_node_id{y = right_wall_calc(Y, ND, Depth)};

right_wall(Point = #ot_node_id{depth = ND, z = Z}, Depth, z) ->
    Point#ot_node_id{z = right_wall_calc(Z, ND, Depth)}.


-spec right_wall_calc(Val :: non_neg_integer(), NodeDepth ::non_neg_integer(),
                      Depth :: non_neg_integer()) -> non_neg_integer().
right_wall_calc(In, ND, Depth) ->
    Mask = ?ALL_BITS_MASK bxor (?ALL_BITS_MASK bsr (Depth-1)),
    NewBits = (?RIGHT_BASE_MASK bsr (Depth - 1))
                  band (?ALL_BITS_MASK bsl (?DEFAULT_MAX_DEPTH - ND)) ,
    ((In band Mask) bor NewBits).



%% prepend_node/2
%% --------------------------------------------------------------------
%% @doc place node identifier before path.
%% @end
-spec prepend_node(Child :: ot_child_code(), Path :: #ot_node_id{}) -> #ot_node_id{}.
%% --------------------------------------------------------------------
prepend_node(Child, #ot_node_id{depth=Depth, x=X, y=Y, z=Z}) ->

    NewX = (xval(Child) bsl (?DEFAULT_MAX_DEPTH -1)) + (X bsr 1),
    NewY = (yval(Child) bsl (?DEFAULT_MAX_DEPTH -1)) + (Y bsr 1),
    NewZ = (zval(Child) bsl (?DEFAULT_MAX_DEPTH -1)) + (Z bsr 1),

    #ot_node_id{depth=Depth+1, x=NewX, y=NewY, z=NewZ}.


%% is_all_zeroes/1
%% --------------------------------------------------------------------
%% @doc return true if the subtree contains only 0's in its components.
%% @end
%% --------------------------------------------------------------------
-spec is_all_zeroes(Point :: ot_node_id()) -> boolean().
%% --------------------------------------------------------------------
is_all_zeroes(#ot_node_id{x=0,y=0,z=0}) -> true;
is_all_zeroes(Val) when is_record(Val, ot_node_id) -> false.


%% is_all_ones/1
%% --------------------------------------------------------------------
%% @doc return true if the subtree contains only 1's in all bits of its components.
%%
%% Note: for a subtree of length 0, the function always will return true,
%% as the needed bits for a path of length 0 is 0.
%%
%% @end
%% --------------------------------------------------------------------
-spec is_all_ones(Point :: #ot_node_id{}) -> boolean().
%% --------------------------------------------------------------------
is_all_ones(#ot_node_id{depth=D, x=X, y=Y, z=Z}) ->
    Sum = bit_count(X) + bit_count(Y) + bit_count(Z),
    (Sum == D*3).

%% common_prefix/2
%% --------------------------------------------------------------------
%% @doc seperate two node specs into a common prefix and subtrees of the rest.
%% @end
-spec common_prefix(#ot_node_id{}, #ot_node_id{}) ->
          {Prefix :: #ot_node_id{}, Rest1 :: #ot_node_id{}, Rest2 :: #ot_node_id{}}.
%% --------------------------------------------------------------------
common_prefix(P1,P2) -> common_prefix(#ot_node_id{depth=0,x=0,y=0,z=0}, P1, P2).

common_prefix(Pre, P1 = #ot_node_id{depth = 0}, P2) -> {Pre, P1, P2};
common_prefix(Pre, P1, P2 = #ot_node_id{depth = 0}) -> {Pre, P1, P2};

common_prefix(Pre, P1, P2) ->
    F1 = first_node(P1),
    F2 = first_node(P2),
    case F1 == F2 of
        true  -> common_prefix(append_node(Pre, F1), rest_nodes(P1), rest_nodes(P2));
        false -> {Pre, P1, P2}
    end.

%% append_node/2
%% --------------------------------------------------------------------
%% @doc place node identifier at end of path
%% @end
-spec append_node(Path :: #ot_node_id{}, Child :: ot_child_code()) -> #ot_node_id{}.
%% --------------------------------------------------------------------
append_node(#ot_node_id{depth=Depth, x=X, y=Y, z=Z}, Child) ->
    Pos = (?DEFAULT_MAX_DEPTH - (Depth + 1)),

    NewX = X + (xval(Child) bsl Pos),
    NewY = Y + (yval(Child) bsl Pos),
    NewZ = Z + (zval(Child) bsl Pos),

    #ot_node_id{depth=Depth+1, x=NewX, y=NewY, z=NewZ}.


%% xval/1
%% --------------------------------------------------------------------
%% @doc compute X value from node identifier
%% @end
-spec xval(Val :: ot_child_code()) -> 0|1.
%% --------------------------------------------------------------------
xval(Val) -> ((Val band ?XMult) div ?XMult).


%% yval/1
%% --------------------------------------------------------------------
%% @doc compute Y value from node identifier
%% @end
-spec yval(Val :: ot_child_code()) -> 0|1.
%% --------------------------------------------------------------------
yval(Val) -> ((Val band ?YMult) div ?YMult).


%% Zval/1
%% --------------------------------------------------------------------
%% @doc compute Z value from node identifier
%% @end
-spec zval(Val :: ot_child_code()) -> 0|1.
%% --------------------------------------------------------------------
zval(Val) -> ((Val band ?ZMult) div ?ZMult).


%% bit_count/1
%% --------------------------------------------------------------------
%% @doc count the number of set bits (i.e the Hamming Weight).
%%
%% This will only count the bits for integers smaller than 2^64
%% see popcount_3 in http://en.wikipedia.org/wiki/Hamming_weight
%%
%% Originaly from: https://gist.github.com/gburd/4955104
%% @end
-spec bit_count( non_neg_integer() | #ot_node_id{}) ->
          non_neg_integer().
%% --------------------------------------------------------------------
bit_count(0) -> 0;
bit_count(X) when is_integer(X), X > 0, X < 16#FFFFFFFFFFFFFFFF -> c6(X);
bit_count(#ot_node_id{x=X,y=Y,z=Z}) -> c6(X) + c6(Y) + c6(Z);
bit_count(Val) -> throw({badarg, Val}).

c1(V) -> V - ((V bsr  1) band 16#5555555555555555).
c2(V) -> ((c1(V) bsr  2) band 16#3333333333333333) + (c1(V) band 16#3333333333333333).
c3(V) -> ((c2(V) bsr  4) + c2(V)) band 16#0F0F0F0F0F0F0F0F.
c4(V) -> ((c3(V) bsr  8) + c3(V)) band 16#00FF00FF00FF00FF.
c5(V) -> ((c4(V) bsr 16) + c4(V)) band 16#0000FFFF0000FFFF.
c6(V) -> ((c5(V) bsr 32) + c5(V)) band 16#00000000FFFFFFFF.


%% normalize/2
%% --------------------------------------------------------------------
%% @doc normalize two points so that point 1 will hold the lesser, point 2
%%      the greater value in all dimensions.
%% @end
-spec normalize(P1 :: PointType, P2 :: PointType) ->
           {P1new :: PointType,  P2new :: PointType} when
    PointType :: #ot_node_id{} | vec_3d().
%% --------------------------------------------------------------------

normalize(#ot_node_id{depth=D1, x=_, y=_, z=_},
          #ot_node_id{depth=D2, x=_, y=_, z=_}) when D1 =/= D2 ->
    throw(different_point_depth_not_supported);

normalize(#ot_node_id{depth=D, x=X1, y=Y1, z=Z1},
          #ot_node_id{depth=D, x=X2, y=Y2, z=Z2}) ->
    {#ot_node_id{depth=D, x= min(X1,X2), y= min(Y1,Y2), z=min(Z1,Z2)},
     #ot_node_id{depth=D, x= max(X1,X2), y= max(Y1,Y2), z=max(Z1,Z2)}};

normalize({X1, Y1, Z1},{X2, Y2, Z2}) ->
    {{min(X1, X2), min(Y1, Y2), min(Z1, Z2)},
     {max(X1, X2), max(Y1, Y2), max(Z1, Z2)}};

normalize(_P1, _P2) ->
    throw(unsupported_point_type_combination).


%% to_node_id/3
%% --------------------------------------------------------------------
%% @private
%% --------------------------------------------------------------------
-spec to_node_id(PosList :: [ot_child_code()],
                 #ot_node_id{},
                 MaxDepth :: non_neg_integer()) -> #ot_node_id{}.
%% --------------------------------------------------------------------
to_node_id([], NodeId, _MaxDepth) -> NodeId;

to_node_id([F|Rest], #ot_node_id{depth = D, x= X, y= Y, z=Z}, MaxDepth) ->
    Shift = MaxDepth - (D + 1),
    Xn = ((F band ?XMult) div ?XMult) bsl Shift,
    Yn = ((F band ?YMult) div ?YMult) bsl Shift,
    Zn = ((F band ?ZMult) div ?ZMult) bsl Shift,
    to_node_id(Rest, #ot_node_id{depth = D + 1, x= X + Xn, y= Y + Yn, z=Z + Zn}, MaxDepth).

%% to_node_list/1
%% --------------------------------------------------------------------
%% @doc convert node id to list of nodes.
%% @end
-spec to_node_list(Point :: #ot_node_id{}) -> [ot_child_code()].
%% --------------------------------------------------------------------
to_node_list(#ot_node_id{depth=0}) -> [];

to_node_list(Point) ->
    F = first_node(Point),
    R = rest_nodes(Point),
    [F | to_node_list(R)].

-define(FIRST_SHIFT,(?DEFAULT_MAX_DEPTH - 1)).
-define(FIRST_MASK,(1 bsl (?DEFAULT_MAX_DEPTH - 1))).

%% first_node/1
%% --------------------------------------------------------------------
%% @doc return the top most element of a tree path.
%%
%%  Example: For a node path [1,2,3] this function would return 1. The
%%  equivalent operation is performed for ot_node_id objects.
%%
%% @end
-spec first_node(PosList :: ot_node_id()) -> ot_child_code() | no_return().
%% --------------------------------------------------------------------
first_node(#ot_node_id{depth=0}) -> throw(zero_depth);

first_node(Point) when is_record(Point, ot_node_id) ->
    (((Point#ot_node_id.x band ?FIRST_MASK) bsr ?FIRST_SHIFT) * ?XMult +
     ((Point#ot_node_id.y band ?FIRST_MASK) bsr ?FIRST_SHIFT) * ?YMult +
     ((Point#ot_node_id.z band ?FIRST_MASK) bsr ?FIRST_SHIFT) * ?ZMult).


%% rest_nodes/1
%% --------------------------------------------------------------------
%% @doc return all but the first element of a pos code
%% TODO: is this almost the same as beyond?
%% @end
-spec rest_nodes(Position :: #ot_node_id{} ) -> #ot_node_id{} | no_return().
%% --------------------------------------------------------------------

rest_nodes(#ot_node_id{depth=0}) -> throw(zero_depth);

rest_nodes(#ot_node_id{depth=Depth, x=X, y=Y, z=Z}) ->
    NewX = ((X band ?QUPOT_REST_MASK) bsl 1),
    NewY = ((Y band ?QUPOT_REST_MASK) bsl 1),
    NewZ = ((Z band ?QUPOT_REST_MASK) bsl 1),
    #ot_node_id{depth=Depth-1, x=NewX , y= NewY, z= NewZ}.


%% default_max_depth/0
%% --------------------------------------------------------------------
%% @private
%% @doc return the default max depth.
%% this is required for building binary expressions.
%% @end
-spec default_max_depth() -> non_neg_integer().
default_max_depth() -> ?DEFAULT_MAX_DEPTH.



%% previous/1
%% --------------------------------------------------------------------
%% @doc get previos node along morton order.
%%
%% Function will return all path elements except the last (i.e the leaf node)
%%
%% Function will throw badargs for any node that has no predecessor.
%%
%% @end
-spec previous(Node :: ot_node_id()) -> ot_node_id().
previous(Node) ->
    try
        Leaf = leaf(Node),
        case Leaf of
            0 -> append_node(previous(inner(Node)), 7);
            L when is_integer(L) -> append_node(inner(Node), L-1)
        end
    catch
        throw:zero_depth -> throw(badargs)
    end.


