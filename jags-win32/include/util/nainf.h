#ifndef NA_INF_H_
#define NA_INF_H_

#ifdef __cplusplus
extern "C" {
#endif

  extern const double JAGS_NA;
  extern const double JAGS_NAN;
  extern const double JAGS_POSINF;
  extern const double JAGS_NEGINF;

  int jags_finite(double);
  int jags_isnan(double);

#ifdef __cplusplus
}
#endif

#endif
