/*
 *
 * WARNING: this code will only work for node ids that are exactly 60 bit long
 * (at max depth).
 *
 */



#include <stdio.h>
#include <erl_nif.h>

#include "CuTest.h"
#include "quperl_octree.h"


//ErlNifUInt64 wall_align(ErlNifUInt64 node_val,
//                        int node_depth,
//                        ErlNifUInt64 point_val) {
//  if (node_val == point_val) return node_val;
//
//  // get delta mask
//  ErlNifUInt64 delta = node_val ^ point_val;
//
//  __builtin_clzl (unsigned long int x)
//
//}

/*
 *    TEST FUNCTIONS
 */
static void Test_get_wall_nif (CuTest* tc)
{
  ErlNifEnv* env = enif_alloc_env ();
  CuAssertPtrNotNull(tc, env);

}

CuSuite* QuperlOctreeNifGetSuite ()
{
  CuSuite* suite = CuSuiteNew ();
//  SUITE_ADD_TEST(suite, Test_get_node_id);
//  SUITE_ADD_TEST(suite, Test_get_wall_nif);
  return suite;
}

