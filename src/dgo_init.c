#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP dgo_dichotomize_cpp(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"dgo_dichotomize_cpp", (DL_FUNC) &dgo_dichotomize_cpp, 1},
    {NULL, NULL, 0}
};

void R_init_dgo(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
