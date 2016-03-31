%%
%% Type definitions for quperl_octree2
%%
%% @author Lutz Behnke <lutz.behnke@gmx.de>
%%
%% @copyright 2015 Lutz Behnke
%%
-ifndef(QUPERL_OCTREE2_HRL).
-define(QUPERL_OCTREE2_HRL, true).

%% use only 60 bit, as erlang can place this in single word on 64bit machines.
-define(DEFAULT_MAX_DEPTH, 60).

%% ((1 bsl (?DEFAULT_MAX_DEPTH-1)) bxor 16#FFFFFFFFFFFFFFFF...)

%%-define(REST_MASK,(16#7fffffffffffffff)).  %% for 64 bit
-define(QUPOT_REST_MASK,(16#7ffffffffffffff)).  %% for 60 bit

-define(XMult, 4).
-define(YMult, 2).
-define(ZMult, 1).

-type ot_child_code() :: 0|1|2|3|4|5|6|7.

-type ot_node_list() :: list(ot_child_code).

-define(DEFAULT_TIMEOUT, infinity).

-type ot_m_code() :: non_neg_integer().

-type vec_3d() :: {float(), float(), float()}.

-type pos_code() :: <<_:8,_:_*8>>.


%% encode a node in octree as depth and three bit strings as integer numbers.
%% the root of the tree is the most significant bit of the integer.
%%
%% this is an abolut positioning from the root of the tree
%%  
-record(ot_node_id, {
                   depth = 0  :: non_neg_integer(),
                   x = 0      :: non_neg_integer(),
                   y = 0      :: non_neg_integer(),
                   z = 0      :: non_neg_integer() 
                  }).

-type ot_node_id() :: #ot_node_id{}.

%% encode a node as a morton code and its depth.
%% morton codes will only enumerate the leaf nodes of a tree, thus the depth
%% is given separately.
-record(ot_morton_node, {
                        depth = 0  :: non_neg_integer(),
                        code  = 0  :: ot_m_code()
                       }).

-type ot_morton_node() :: #ot_morton_node{}.

-record(ot_morton_interval, {
                        depth = 0  :: non_neg_integer(),
                        interval  :: {ot_m_code(), ot_m_code()}
                       }).

-type ot_morton_interval() :: #ot_morton_interval{}.

-record(ot_volume, {
                 max_depth = ?DEFAULT_MAX_DEPTH :: non_neg_integer(),
                 spaces    = []                 :: [ ot_node_id()
                                                   | ot_morton_interval()
                                                   | ot_morton_node() 
                                                   ]
                 }).

-type ot_volume() :: #ot_volume{}.


-endif. %% QUPERL_OCTREE_HRL
