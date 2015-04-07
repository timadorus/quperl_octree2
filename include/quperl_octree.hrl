%%
%% Type definitions for quperl_octree2
%%
%% @author Lutz Behnke <lutz.behnke@gmx.de>
%%
%% @copyright 2015 Lutz Behnke
%%
-ifndef(QUPERL_OCTREE_HRL).
-define(QUPERL_OCTREE_HRL, true).

-define(DEFAULT_MAX_DEPTH, 64).

-type m_interval() :: {non_neg_integer(), non_neg_integer()}.
-type vec_3d() :: {float(), float(), float()}.


-record(octree, {
                 max_depth = ?DEFAULT_MAX_DEPTH :: pos_integer(),
                 spaces    = []                 :: [ non_neg_integer() | m_interval() ]
                 }).

-type octree() :: #octree{}.

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

-endif. %% QUPERL_OCTREE_HRL
