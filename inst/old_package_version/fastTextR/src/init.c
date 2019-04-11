#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _fastTextR_convert_args_to_pointers(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_fastTextR_convert_args_to_pointers", (DL_FUNC) &_fastTextR_convert_args_to_pointers, 3},
    {NULL, NULL, 0}
};

void R_init_fastTextR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}