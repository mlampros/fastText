#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _fastText_give_args_fasttext(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _fastText_printAnalogiesUsage();
extern SEXP _fastText_printDumpUsage();
extern SEXP _fastText_printNNUsage();
extern SEXP _fastText_printPredictUsage();
extern SEXP _fastText_printPrintNgramsUsage();
extern SEXP _fastText_printPrintSentenceVectorsUsage();
extern SEXP _fastText_printPrintWordVectorsUsage();
extern SEXP _fastText_printQuantizeUsage();
extern SEXP _fastText_printTestLabelUsage();
extern SEXP _fastText_printTestUsage();
extern SEXP _fastText_printUsage();

static const R_CallMethodDef CallEntries[] = {
    {"_fastText_give_args_fasttext",             (DL_FUNC) &_fastText_give_args_fasttext,             6},
    {"_fastText_printAnalogiesUsage",            (DL_FUNC) &_fastText_printAnalogiesUsage,            0},
    {"_fastText_printDumpUsage",                 (DL_FUNC) &_fastText_printDumpUsage,                 0},
    {"_fastText_printNNUsage",                   (DL_FUNC) &_fastText_printNNUsage,                   0},
    {"_fastText_printPredictUsage",              (DL_FUNC) &_fastText_printPredictUsage,              0},
    {"_fastText_printPrintNgramsUsage",          (DL_FUNC) &_fastText_printPrintNgramsUsage,          0},
    {"_fastText_printPrintSentenceVectorsUsage", (DL_FUNC) &_fastText_printPrintSentenceVectorsUsage, 0},
    {"_fastText_printPrintWordVectorsUsage",     (DL_FUNC) &_fastText_printPrintWordVectorsUsage,     0},
    {"_fastText_printQuantizeUsage",             (DL_FUNC) &_fastText_printQuantizeUsage,             0},
    {"_fastText_printTestLabelUsage",            (DL_FUNC) &_fastText_printTestLabelUsage,            0},
    {"_fastText_printTestUsage",                 (DL_FUNC) &_fastText_printTestUsage,                 0},
    {"_fastText_printUsage",                     (DL_FUNC) &_fastText_printUsage,                     0},
    {NULL, NULL, 0}
};

void R_init_fastText(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
