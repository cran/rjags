#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> 
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP adapt_off(SEXP);
extern SEXP check_adaptation(SEXP);
extern SEXP check_model(SEXP, SEXP);
extern SEXP clear_console(SEXP);
extern SEXP clear_monitor(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP compile(SEXP, SEXP, SEXP, SEXP);
extern SEXP get_data(SEXP);
extern SEXP get_factories(SEXP);
extern SEXP get_iter(SEXP);
extern SEXP get_modules();
extern SEXP get_monitored_values(SEXP, SEXP);
extern SEXP get_monitored_values_flat(SEXP, SEXP);
extern SEXP get_nchain(SEXP);
extern SEXP get_samplers(SEXP);
extern SEXP get_state(SEXP);
extern SEXP get_variable_names(SEXP);
extern SEXP get_version();
extern SEXP initialize(SEXP);
extern SEXP is_adapting(SEXP);
extern SEXP load_module(SEXP);
extern SEXP make_console();
extern SEXP parallel_seeds(SEXP, SEXP);
extern SEXP quietMessages(SEXP);
extern SEXP set_factory_active(SEXP, SEXP, SEXP);
extern SEXP set_monitors(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP set_parameters(SEXP, SEXP, SEXP);
extern SEXP set_rng_name(SEXP, SEXP, SEXP);
/* extern SEXP set_seed(SEXP); JAGS >= 5.0 */
extern SEXP unload_module(SEXP);
extern SEXP update(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"adapt_off",                 (DL_FUNC) &adapt_off,                 1},
    {"check_adaptation",          (DL_FUNC) &check_adaptation,          1},
    {"check_model",               (DL_FUNC) &check_model,               2},
    {"clear_console",             (DL_FUNC) &clear_console,             1},
    {"clear_monitor",             (DL_FUNC) &clear_monitor,             5},
    {"compile",                   (DL_FUNC) &compile,                   4},
    {"get_data",                  (DL_FUNC) &get_data,                  1},
    {"get_factories",             (DL_FUNC) &get_factories,             1},
    {"get_iter",                  (DL_FUNC) &get_iter,                  1},
    {"get_modules",               (DL_FUNC) &get_modules,               0},
    {"get_monitored_values",      (DL_FUNC) &get_monitored_values,      2},
    {"get_monitored_values_flat", (DL_FUNC) &get_monitored_values_flat, 2},
    {"get_nchain",                (DL_FUNC) &get_nchain,                1},
    {"get_samplers",              (DL_FUNC) &get_samplers,              1},
    {"get_state",                 (DL_FUNC) &get_state,                 1},
    {"get_variable_names",        (DL_FUNC) &get_variable_names,        1},
    {"get_version",               (DL_FUNC) &get_version,               0},
    {"initialize",                (DL_FUNC) &initialize,                1},
    {"is_adapting",               (DL_FUNC) &is_adapting,               1},
    {"load_module",               (DL_FUNC) &load_module,               1},
    {"make_console",              (DL_FUNC) &make_console,              0},
    {"parallel_seeds",            (DL_FUNC) &parallel_seeds,            2},
    {"quietMessages",             (DL_FUNC) &quietMessages,             1},
    {"set_factory_active",        (DL_FUNC) &set_factory_active,        3},
    {"set_monitors",              (DL_FUNC) &set_monitors,              6},
    {"set_parameters",            (DL_FUNC) &set_parameters,            3},
    {"set_rng_name",              (DL_FUNC) &set_rng_name,              3},
/*  {"set_seed",                  (DL_FUNC) &set_seed,                  1}, */
    {"unload_module",             (DL_FUNC) &unload_module,             1},
    {"update",                    (DL_FUNC) &update,                    2},
    {NULL, NULL, 0}
};

void R_init_rjags(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}


