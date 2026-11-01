/*
 *  R package rjags file src/flags.c
 *  Copyright (C) 2006-2026 Martyn Plummer and Matt Denwood
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  version 2 as published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  A copy of the GNU General Public License is available at
 *  http://www.r-project.org/Licenses/
 */

#include <R.h>
#include <Rdefines.h>

// These should be injected by Makevars:
#ifndef RJAGS_CPPFLAGS
#error "Compilation failed: RJAGS_CPPFLAGS undefined"
#endif
#ifndef RJAGS_CXXFLAGS
#error "Compilation failed: RJAGS_CXXFLAGS undefined"
#endif
#ifndef RJAGS_LIBS
#error "Compilation failed: RJAGS_LIBS undefined"
#endif
#ifndef JAGS_ROOT
#error "Compilation failed: JAGS_ROOT undefined"
#endif

#define STRINGIFY(x) #x
#define SHIM_STRING(x) STRINGIFY(x)

SEXP get_rjags_cppflags() {
  return Rf_mkString(SHIM_STRING(RJAGS_CPPFLAGS));
}

SEXP get_rjags_cxxflags() {  
  return Rf_mkString(SHIM_STRING(RJAGS_CXXFLAGS));
}

SEXP get_rjags_libs() {  
  return Rf_mkString(SHIM_STRING(RJAGS_LIBS));
}

SEXP get_jags_root() {  
  return Rf_mkString(SHIM_STRING(JAGS_ROOT));
}
