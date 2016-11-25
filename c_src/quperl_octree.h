
#ifndef QUPERL_OCTREE_H
#defined QUPERL_OCTREE_H


#define true 1
#define false 0

#define NODE_ID_ARITY 5
#define NODE_ID_RECORD_NAME "ot_node_id"

#define REST_MASK 0x7ffffffffffffff

typedef struct
{
  unsigned int depth,
  ErlNifUInt64 x, y, z;
} t_node_id;


ErlNifUInt64 wall_align(ErlNifUInt64 node_val, int node_depth, ErlNifUInt64depth point_val);

#endif QUPERL_OCTREE_H
