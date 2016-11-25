/*
 * NIF functions to optimise bit-field handling.
 */

#include <stdio.h>

#include "erl_nif.h"
#include "quperl_octree.h"


/*
 *   PROTOTYPES
 */

/* Read erlang ot_node_id recod into c structure.
 *
 *  function will either fill node_id structure completely or not at all.
 */
static int get_node_id (ErlNifEnv* env, ERL_NIF_TERM term, t_node_id* node_id)
{

  const ERL_NIF_TERM *node_id_term;
  ErlNifUInt64 depth, x, y, z;
  int actual_arity;

  if (!enif_get_tuple (env, term, &actual_arity, &node_id_term))
    return false;

  if (NODE_ID_ARITY != actual_arity) {
//    printf("actual arity is %i\n\n",actual_arity);
    return false;
  }

  if (!enif_get_uint64 (env, node_id_term[1], &depth))
    return false;

  if (!enif_get_uint64 (env, node_id_term[2], &x))
    return false;

  if (!enif_get_uint64 (env, node_id_term[3], &y))
    return false;

  if (!enif_get_uint64 (env, node_id_term[4], &z))
    return false;

  node_id->depth = depth;
  node_id->x = x;
  node_id->y = y;
  node_id->z = z;

  return true;
}

static ERL_NIF_TERM make_node_id_record (ErlNifEnv* env, t_node_id* node_id)
{
  ERL_NIF_TERM ot_node_id_atom, depth_val, x_val, y_val, z_val;

  if (!enif_make_existing_atom (env, NODE_ID_RECORD_NAME, &ot_node_id_atom,
                                ERL_NIF_LATIN1))
    {
      return enif_make_badarg (env);
    }

  depth_val = enif_make_int64 (env, node_id->depth);
  x_val = enif_make_int64 (env, node_id->x);
  y_val = enif_make_int64 (env, node_id->y);
  z_val = enif_make_int64 (env, node_id->z);

  return enif_make_tuple5 (env, ot_node_id_atom, depth_val, x_val, y_val, z_val);
}

/* compute the point still within Node, but as close to P as possible
 *
 * Parameters:
 *      Node: tuple(ot_node_id, depth: long int, X: long int,Y: long int,Z: long int )
 *
 *
 */
static ERL_NIF_TERM wall_align_nif (ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  t_node_id node, point, ret_node;

  if (!get_node_id (env, argv[0], &node))
    {
      return enif_make_badarg (env);
    }

  if (!get_node_id (env, argv[1], &point))
    {
      return enif_make_badarg (env);
    }

  ret_node.depth = node.depth;
  ret_node.x     = node.x;
  ret_node.y     = node.y;
  ret_node.z     = node.z;

  return make_node_id_record (env, &ret_node);
}

/*
 *     LOAD FUNCTIONS INTO ERLANG
 */

static ErlNifFunc nif_funcs[] =
  {
    { "wall_align", 2, &wall_align_nif }
  };

ERL_NIF_INIT ( quperl_fast_box_to_volume, nif_funcs, NULL, NULL, NULL, NULL)


