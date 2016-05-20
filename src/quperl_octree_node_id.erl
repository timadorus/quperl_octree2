%%
%% @doc nodes in octree and functions that operate on them.
%%
%% @see for discussion on basic concept.
%%
%% @author Lutz Behnke <lutz.behnke@gmx.de>
%%
%% @copyright Lutz Behnke 2015-2016
%%

-module(quperl_octree_node_id).

-include_lib("eunit/include/eunit.hrl").
-include("quperl_octree.hrl").

%% ====================================================================
%% API functions
%% ====================================================================


-export([to_node_id/1, to_node_id/2, is_node_id/1,
         to_node_list/1, inner/1, leaf/1,
         is_equal/2, get_depth/1, get_value/2,
         children/1, for_each_child/3, xval/1, yval/1, zval/1, append_node/2,
         prepend_node/2, normalize/2, common_prefix/2,
         first_node/1, rest_nodes/1]).


-ifdef(TEST).
%% export the private functions for testing only.
-export([previous/1,
         default_max_depth/0,
         is_ancestor_of/2, beyond/2
        ]).
-endif.

%% to_node_id/3,
%%          box_to_volume/1, box_to_volume/2, filter_full_area/1,
%%
%%
%%          split/4,
%%          sweep/1,
%%


%% is_node_id/1
%% --------------------------------------------------------------------
%% @doc test for node_id.
%%
%% return true if the argument is a node_id, false if otherwise.
%% @end
-spec is_node_id(term()) -> boolean().
%% --------------------------------------------------------------------
is_node_id(Arg) when is_record(Arg, ot_node_id) -> true;

is_node_id(_) -> false.

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


-dialyzer({nowarn_function, children/1}).

%% children/1
%% --------------------------------------------------------------------
%% @doc return the list of children of a node
%% @end
%% --------------------------------------------------------------------
-spec children(Node :: quperl_octree:node_id()) -> [quperl_octree:node_id()].
children(Node) ->
    for_each_child(Node, fun(Child, _Extra) -> [Child] end, []).


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


%% for_each_child/3
%% --------------------------------------------------------------------
%% @doc execute function on each child of a node.
%% @end
%% --------------------------------------------------------------------
-spec for_each_child(N :: quperl_octree:node_id(),
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


