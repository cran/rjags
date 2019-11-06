#ifndef INIT_H_
#define INIT_H_

#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif
    
    extern SEXP adapt_off(SEXP);
    extern SEXP check_adaptation(SEXP);
    extern SEXP check_model(SEXP, SEXP);
    extern SEXP clear_console(SEXP);
    extern SEXP clear_monitor(SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP compileR(SEXP, SEXP, SEXP, SEXP);
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

#ifdef __cplusplus
}
#endif

#endif /* INIT_H_ */

