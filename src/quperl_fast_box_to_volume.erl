%% @author sage
%% @doc and yet another approach to constructing a list of octree nodes from an AABB.
%%
%% If P1 and P2 have common prefix, N0 = prefix, otherwise N0 is root.
%%
%% for a given node: N, constructing for normalized points (P1, P2)
%%  partition(N, P1, P2) ->
%%   - if N0 is ancestor of neither point, return []
%%   - if N0 is ancestor of both points, 
%%          constuct minimal and maximal points P_min, P_max for N,
%%          - if (P1 == P_min) and (P2 == P_max) return [N]
%%   - else 
%%       for N' -> Nc0, NC2....NC7 for each child of N
%%         (P1', P2') = border_align(N',P1,P2)
%%         partition(N',P1',P2')
%%       return concatenated list of results.
%% 
%% border_align(N,P1,P2) will return either P1 or P2 if they are within N and 
%%   replace the other point with a new point that is positioned as far as 
%%   
possible to wards the original point 
%%
repeat with each child, returning  


-module(quperl_fast_box_to_volume).

%% ====================================================================
%% API functions
%% ====================================================================
-export([box_to_volume/2]).

%% new_box_to_volume/2
%% --------------------------------------------------------------------
%% @doc convert an axis aligned bounding box to a list of node ids.
%% @end
%% --------------------------------------------------------------------
-spec box_to_volume(P1 :: ot_node_id(), P2 :: ot_node_id()) -> [ot_node_id()].
box_to_volume(P1, P2) when P1#ot_node_id.depth == P2#ot_node_id.depth ->
    new_box_to_volume(#ot_node_id{}, P1, P2).


%% ====================================================================
%% Internal functions
%% ====================================================================


