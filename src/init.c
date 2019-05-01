
#include <R_ext/Rdynload.h>

#include "MSExample.h"

static const R_CallMethodDef callMethods[] = {
    {"C_locmax", (DL_FUNC) &C_locmax, 2},
    {NULL, NULL, 0}
};

void R_init_MSExample(DllInfo * info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
