/*
 * NIF functions to optimise bit-field handling.
 */

#include "erl_nif.h"

extern int foo(int x);
extern int bar(int y);

static ErlNifFunc nif_funcs[] = {
    {"wall_align_nif", 2, wall_align_nif}
};

ERL_NIF_INIT(quperl_octree, nif_funcs, NULL, NULL, NULL, NULL)

/* compute the point still within Node, but as close to P as possible
 *
 * Parameters:
 *      P
 *
 *
 */
static ERL_NIF_TERM wall_align_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int
  if (!enif_get_int(env, argv[0], &x)) {
      return enif_make_badarg(env);
  }

  ret = foo(x);
  return enif_make_int(env, ret);
}

static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x, ret;
    if (!enif_get_int(env, argv[0], &x)) {
    return enif_make_badarg(env);
    }
    ret = foo(x);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM bar_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int y, ret;
    if (!enif_get_int(env, argv[0], &y)) {
    return enif_make_badarg(env);
    }
    ret = bar(y);
    return enif_make_int(env, ret);
}



void Testfoo_nif(CuTest *tc) {
        char* input = strdup("hello world");
        char* actual = StrToUpper(input);
        char* expected = "HELLO WORLD";
        CuAssertStrEquals(tc, expected, actual);
    }

CuSuite* StrUtilGetSuite() {
        CuSuite* suite = CuSuiteNew();
        SUITE_ADD_TEST(suite, TestStrToUpper);
        return suite;
    }

