%%
%% @doc manage arbitrary volumes within octree.
%%
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
-export([new_volume/0, new_volume/1, new_volume/2,
         to_node_id/1, to_node_id/2,
         to_node_list/1, inner/1, leaf/1,
         is_equal/2]).


-ifdef(TEST).
%% export the private functions for testing only.
-export([normalize/2, to_node_id/3,
         box_to_volume/1, box_to_volume/2, filter_full_area/1,
         common_prefix/2, first_node/1, rest_nodes/1,
         append_node/2, xval/1, yval/1, zval/1, is_all_ones/1,
         is_all_zeroes/1, bit_count/1, default_max_depth/0, split/4,
         xor_dim/3, sweep/1, previous/1, new_box_to_volume/2,
         for_each_child/2, is_parent_of/2
        ]).
-endif.

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
    new_volume(box_to_volume([normalize(Point1,Point2)]));

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
-spec leaf(Point :: #ot_node_id{}) -> ot_child_code() | no_return().
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
    Mask = (?RIGHT_SHIFT_MASK bxor (1 bsl Pos)),

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



%% for_each_child/2
%% --------------------------------------------------------------------
%% @doc execute function on each child of a node.
%% @end
%% --------------------------------------------------------------------
-spec for_each_child(N :: #ot_node_id{}, F :: fun((Child :: #ot_node_id{}) -> term()) ) -> [term()].
for_each_child(Node, Fun) ->
    lists:map(fun(L) -> Fun(append_node(Node,L)) end, lists:seq(0, 7)).


%% is_parent_of/2
%% --------------------------------------------------------------------
%% @doc return if N1 is parent of N2, false otherwise.
%% @end
%% --------------------------------------------------------------------
-spec is_parent_of(N1 :: #ot_node_id{}, N2 :: #ot_node_id{}) -> boolean().

is_parent_of(P1, P2) when P1#ot_node_id.depth >= P2#ot_node_id.depth ->
    false;

is_parent_of(P1, P2)  ->
    D = P1#ot_node_id.depth,

    is_parent_of(P1#ot_node_id.x, P2#ot_node_id.x, D)
        and is_parent_of(P1#ot_node_id.y, P2#ot_node_id.y, D)
        and is_parent_of(P1#ot_node_id.z, P2#ot_node_id.z, D).

is_parent_of(N1, N2, D) ->
    Mask = bnot (?RIGHT_SHIFT_MASK bsr D),
    N1 == (N2 band Mask).


%% new_box_to_volume/2
%% --------------------------------------------------------------------
%% @doc convert an axis aligned bounding box to a list of node ids.
%% @end
%% --------------------------------------------------------------------
-spec new_box_to_volume(P1 :: ot_node_id(), P2 :: ot_node_id()) -> [ot_node_id()].
new_box_to_volume(P1, P2) ->
    new_box_to_volume(#ot_node_id{}, P1, P2).


%% new_box_to_volume/3
%% --------------------------------------------------------------------
%% @doc find nodes that are part of volume in sub-tree that has node as
%%  root.
%% @end
%% --------------------------------------------------------------------
-spec new_box_to_volume(N :: ot_node_id(), P1 :: ot_node_id(), P2 :: ot_node_id()) -> [ot_node_id()].
new_box_to_volume(Parent, Point1, Point2) ->

    for_each_child(Parent,
                   fun(Node) ->
                      Parent1 = is_parent_of(Node, Point1),
                      Parent2 = is_parent_of(Node, Point2),
                      handle_node_parent(Parent1, Parent2, Node, Point1, Point2)
                           end),
    ok.


%% neither point is in node: ignore
handle_node_parten(false, false, _Node, _Point1, _Point2) -> [];

handle_node_parten(true, false, Node, Point1, Point2) ->
    ok;

handle_node_parten(false, true, Node, Point1, Point2) ->
    ok;



%% box_to_volume/1
%% --------------------------------------------------------------------
%% @doc convert an axis aligned bounding box to a list of node ids.
%% @end
%% --------------------------------------------------------------------
-spec box_to_volume(Inlist :: [{ot_node_id(), ot_node_id()}]) ->
          [ot_node_id()].
box_to_volume(InList) -> sweep(box_to_volume(InList, [])).

%% box_to_volume/2
%% --------------------------------------------------------------------
%% @doc convert an axis aligned bounding box to a list of node ids.
%% @end
%% --------------------------------------------------------------------
-spec box_to_volume(Inlist :: [{ot_node_id(), ot_node_id()}],
                    OutList :: [{ot_node_id(), ot_node_id()}]) ->
          [ot_node_id()].
box_to_volume([], Result) -> Result;

box_to_volume([{P1,P2}| Rest], Result) ->
    case is_equal(P1, P2) of
        true  -> box_to_volume(Rest, [P1|Result]);

        false -> SplitList = split([], ?DEFAULT_MAX_DEPTH, P1, P2),

%%                  ?debugFmt("SubTrees Len: ~p", [length(SplitList)]),

                 {SplitList2, Areas} = filter_full_area(SplitList),

%%                  ?debugFmt("Rest Len: ~p, SplitList Len: ~p, Result Len: ~p, Areas Len: ~p",
%%                            [length(Rest), length(SplitList2), length(Result), length(Areas)]),

                 box_to_volume(Rest ++ SplitList2, Result ++ Areas)
    end.

%% split/4
%% --------------------------------------------------------------------
%% @doc split along topmost 'border' in tree
%%
%% Precondition: P1 =/= P2
%%
%% @end
-spec split(TreePrefix :: [ot_child_code()], RestDepth :: non_neg_integer(),
            P1 :: #ot_node_id{}, P2 :: #ot_node_id{}) ->
          [ {#ot_node_id{}, #ot_node_id{}} ].
%% --------------------------------------------------------------------
split(TreePrefix, RestDepth, P1, P2) ->

    F1 = first_node(P1), F2 = first_node(P2),

%%     ?debugFmt("TreePrefix: ~p, RestDepth: ~p, F1: ~p, F2: ~p, P1: ~p, P2: ~p",[TreePrefix, RestDepth, F1, F2, P1, P2]),

    case F1 == F2 of
        true ->
            R1 = rest_nodes(P1), R2 = rest_nodes(P2),
            split(TreePrefix ++ [F1], RestDepth-1, R1, R2);
        false ->
            List1 = split_borders([{P1,P2}],x),
            List2 = split_borders(List1,y),
            List3 = split_borders(List2,z),

            lists:map(fun({Node1, Node2}) ->
                              {prepend_path(Node1, TreePrefix),
                               prepend_path(Node2, TreePrefix)}
                      end, List3)
    end.

%% split_borders/2
%% --------------------------------------------------------------------
%% @doc split a number of segments in a list.
%%
%% @end
-spec split_borders(Segments :: [ {#ot_node_id{}, #ot_node_id{}} ],
                    Dim :: x|y|z) ->
          [ {#ot_node_id{}, #ot_node_id{}} ].
%% --------------------------------------------------------------------
split_borders(Segments, Dim) ->
    lists:foldl(fun({P1, P2},Acc) ->
                        Acc ++ split_border(P1,P2,Dim)
                        end, [], Segments).

%% split_border/3
%% --------------------------------------------------------------------
%% @doc split segment along top most border based on given dimension.
%%
%%   the points must not have a common prefix.
%%
%% @end
-spec split_border(P1 :: #ot_node_id{}, P2 :: #ot_node_id{}, Dim :: x|y|z) ->
          [{#ot_node_id{},#ot_node_id{}}].
%% --------------------------------------------------------------------
split_border(P1,P2,Dim) ->
    F1 = first_node(P1),
    F2 = first_node(P2),

%%     ?debugFmt("F1: ~p, F2: ~p", [F1, F2]),

    case xor_dim(F1,F2,Dim) of
     %% both areas are the same
      0  ->  [{P1, P2}];
     %% areas are different
      1  ->  [{P1, right_wall(P2,Dim)},{left_wall(P1,Dim), P2}]
    end.


-define(LEFT_WALL, 1 bsl (?DEFAULT_MAX_DEPTH - 1)).

%% left_wall/2
%% --------------------------------------------------------------------
%% @doc set all bits but the first of given dimension to 0, thus following the left
%%      wall of the octree area.
%% @end
-spec left_wall(#ot_node_id{}, x|y|z) -> #ot_node_id{}.
%% --------------------------------------------------------------------
left_wall(Point, x) when is_record(Point, ot_node_id) ->
    Point#ot_node_id{x = ?LEFT_WALL};

left_wall(Point, y) when is_record(Point, ot_node_id)  ->
    Point#ot_node_id{y = ?LEFT_WALL};

left_wall(Point, z) when is_record(Point, ot_node_id)  ->
    Point#ot_node_id{z = ?LEFT_WALL}.


%% right_wall/2
%% --------------------------------------------------------------------
%% @doc set all bits but the first of given dimension to one, thus following the right
%%      wall of the octree area.
%% @end
-spec right_wall(#ot_node_id{}, x|y|z) -> #ot_node_id{}.
%% --------------------------------------------------------------------
right_wall(Point = #ot_node_id{depth = D}, x) ->
    Val = ?RIGHT_BASE_MASK band (?RIGHT_SHIFT_MASK bsl (?DEFAULT_MAX_DEPTH - D)),
    Point#ot_node_id{x = Val};

right_wall(Point = #ot_node_id{depth = D}, y) ->
    Val = ?RIGHT_BASE_MASK band (?RIGHT_SHIFT_MASK bsl (?DEFAULT_MAX_DEPTH - D)),
    Point#ot_node_id{y = Val};

right_wall(Point = #ot_node_id{depth = D}, z) ->
    Val = ?RIGHT_BASE_MASK band (?RIGHT_SHIFT_MASK bsl (?DEFAULT_MAX_DEPTH - D)),
    Point#ot_node_id{z = Val}.

%% prepend_path/2
%% --------------------------------------------------------------------
%% @doc prepend a path segment to a node.
%% @end
-spec prepend_path(Node :: #ot_node_id{}, Prefix :: [ ot_child_code()]) -> #ot_node_id{}.
%% --------------------------------------------------------------------
prepend_path(Node, []) -> Node;

prepend_path(Node, [First|Rest]) ->
    prepend_node(First, prepend_path(Node, Rest)).

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


%% filter_full_area/1
%% --------------------------------------------------------------------
%% @doc seperate fully filled areas from the candates for further splitting.
%%
%% Will return a tuple containing a) a list of segments that do not fully fill a node yet,
%% b) a list of nodes
%%
%%
%% an area is fully filled if
%%
%% @end
-spec filter_full_area(SplitList :: [{ot_node_id(), ot_node_id()}]) ->
          {ListList :: [{ot_node_id(), ot_node_id()}], Results :: [ot_node_id()]}.
%% --------------------------------------------------------------------
filter_full_area(SplitList) ->
    filter_full_area(SplitList,[],[]).

filter_full_area([], SplitOut, Results) -> {SplitOut, Results};

filter_full_area([{P1,P2}|SplitListRest], SplitOut, Results) ->
    {Prefix, Rest1, Rest2} = common_prefix(P1,P2),

    %% check if P1 is the smallest and P2 the greatest possible corner.
    case (is_all_zeroes(Rest1) and is_all_ones(Rest2)) of
        true  -> filter_full_area(SplitListRest, SplitOut, Results ++ [Prefix]);
        false -> filter_full_area(SplitListRest, [{P1,P2} | SplitOut], Results)
    end.


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
-spec first_node(PosList :: #ot_node_id{}) -> ot_child_code() | no_return().
%% --------------------------------------------------------------------
first_node(#ot_node_id{depth=0}) -> throw(zero_depth);

first_node(Point) when is_record(Point, ot_node_id) ->
    (((Point#ot_node_id.x band ?FIRST_MASK) bsr ?FIRST_SHIFT) * ?XMult +
     ((Point#ot_node_id.y band ?FIRST_MASK) bsr ?FIRST_SHIFT) * ?YMult +
     ((Point#ot_node_id.z band ?FIRST_MASK) bsr ?FIRST_SHIFT) * ?ZMult).


%% rest_nodes/1
%% --------------------------------------------------------------------
%% @doc return all but the first element of a pos code
%% @end
-spec rest_nodes(Position :: #ot_node_id{} ) -> #ot_node_id{} | no_return().
%% --------------------------------------------------------------------

rest_nodes(#ot_node_id{depth=0}) -> throw(zero_depth);

rest_nodes(#ot_node_id{depth=Depth, x=X, y=Y, z=Z}) ->
    NewX = ((X band ?QUPOT_REST_MASK) bsl 1),
    NewY = ((Y band ?QUPOT_REST_MASK) bsl 1),
    NewZ = ((Z band ?QUPOT_REST_MASK) bsl 1),
    #ot_node_id{depth=Depth-1, x=NewX , y= NewY, z= NewZ}.

%% xor_dim/3
%% --------------------------------------------------------------------
%% @doc return xor on the bit defined by the dimension
%% @end
-spec xor_dim(Pos1 :: 0|1|2|3|4|5|6|7, Pos2 :: 0|1|2|3|4|5|6|7,
              Dim :: x|y|z) -> 0|1.
%% --------------------------------------------------------------------

xor_dim(Pos1, Pos2, x) -> ((Pos1 band ?XMult) bxor (Pos2 band ?XMult)) bsr 2;
xor_dim(Pos1, Pos2, y) -> ((Pos1 band ?YMult) bxor (Pos2 band ?YMult)) bsr 1;
xor_dim(Pos1, Pos2, z) -> ((Pos1 band ?ZMult) bxor (Pos2 band ?ZMult)) bsr 0.

%% default_max_depth/0
%% --------------------------------------------------------------------
%% @private
%% @doc return the default max depth.
%% this is required for building binary expressions.
%% @end
-spec default_max_depth() -> pos_integer().
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
-spec previous(Node :: #ot_node_id{}) -> #ot_node_id{}.
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


%% sweep/1
%% --------------------------------------------------------------------
%% @doc collapse nodes in list that completely cover a node.
%%
%% PRE: the nodes are sorted according to the Morton order.
%%
%% the function will replace all nodes that constituate a complete node by this node.
%%
%% @private
%% @end
-spec sweep(Nodes :: [#ot_node_id{}] ) -> [#ot_node_id{}].
%% --------------------------------------------------------------------
sweep([]) -> [];

sweep(Nodes) ->

    {LastQ, Ret} =
        lists:foldl(fun(Elem, {Q, Out})->
                            case queue:peek_r(Q) of
                                %% special case: first element
                                empty ->
                                    {queue:in(Elem, Q), Out};
                                {value, Last} ->
                                    case previous(Elem) of
                                        Last ->
                                            case queue:len(Q) of
                                                7 ->
                                                    {queue:new(), Out ++ [inner(Elem)]};
                                                _ ->
                                                    {queue:in(Elem, Q), Out}
                                            end;
                                        _Val ->
                                            %% place elem in new queue, put content of
                                            %% of old queue at the end of out
                                            {queue:in(Elem, queue:new()), Out ++ queue:to_list(Q)}
                                    end
                            end
                    end, {queue:new(), []}, Nodes),

Ret ++ queue:to_list(LastQ).
