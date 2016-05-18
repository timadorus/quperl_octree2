%% @author sage
%% @doc compute and manage a binary position code for a node in an octree.
%%
%% <p>Functions to compute and modify an position code, which identifies a specific
%% node in an octree and is encoded as a binary string, to be used as a key for
%% object lookups.
%%
%% <p>The binary is structured as follows:
%% &lt;&lt;Depth/8,PathCode/3*DEFAULT_MAX_DEPTH&gt;&gt;
%%
%% should (DEFAULT_MAX_DEPTH mod 8 =/= 0) be true, the binary will be padded to the
%% next full byte.
%%
%% <p>Depth indicates the depth of the tree that was traversed, using 3*Depth
%% bits in the path. The rest will be padded with 0s.
%%
%% <p>DEFAULT_MAX_DEPTH currently is fixed at 60, allowing the smallest
%% node of the octree to be 2^60 ~= 10^24 times smaller than the space
%% encompassed by the whole tree.</p>
%%
%% <p>The values for each coordinate c of the position and size are assumed to
%% be normalized to be within the interval ( 0.0 <= c < 1.0 )</p>
%% @end


-module(quperl_octree_pos_code).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-include("quperl_octree.hrl").



%% ====================================================================
%% API functions
%% ====================================================================
-export([is_pos_code/1, to_pos_code/1, to_pos_code/2]).

%% is_pos_code/1
%% --------------------------------------------------------------------
%% @doc verify that argument is positional code.
%%
%% will return true if argument is a valid pos code, false otherwise.
%%
-spec is_pos_code(PosCode:: binary()) -> true | false.
%% --------------------------------------------------------------------
is_pos_code(<<Depth:8,
              _PathCode:(3*?DEFAULT_MAX_DEPTH)/bits,
              _Padding:((8-((3*?DEFAULT_MAX_DEPTH) rem 8)) rem 8)/bits>>)
  when Depth > ?DEFAULT_MAX_DEPTH -> false;

is_pos_code(<<_Depth:8,
              _PathCode:(3*?DEFAULT_MAX_DEPTH)/bits,
              _Padding:((8-((3*?DEFAULT_MAX_DEPTH) rem 8)) rem 8)/bits>>)  -> true;

is_pos_code(_Other) -> false.


%% to_pos_code/1
%% --------------------------------------------------------------------
%% @doc create pos code from node specification.
%%
%% @end
-spec to_pos_code(#ot_node_id{}) -> pos_code().
%% --------------------------------------------------------------------
to_pos_code(NId) -> to_pos_code(NId, ?DEFAULT_MAX_DEPTH).

%% to_pos_code/2
%% --------------------------------------------------------------------
%% @doc create pos code from node specification.
%%
%% @end
-spec to_pos_code(#ot_node_id{}, MaxDepth :: pos_integer()) -> pos_code().
%% --------------------------------------------------------------------
to_pos_code(NId, MaxDepth) ->
    Spec = quperl_octree_node_id:to_node_list(NId),

    {Depth, PosCode} = lists:foldl(fun add_pos_code/2, {0,<<>>}, Spec),

    <<Depth,
      PosCode/bits, 0:((MaxDepth-Depth)*3),
      0:((8-((3*MaxDepth) rem 8)) rem 8)>>.




%% ====================================================================
%% Internal functions
%% ====================================================================

add_pos_code(Elem, {Count, Tail}) ->
    {Count +1, <<Tail/bits,Elem:3>>}.


