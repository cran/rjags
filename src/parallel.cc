/*
 *  R package rjags file src/parallel.cc 
 *  Copyright (C) 2011-2013 Martyn Plummer
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


/* Utility function to get parallel seeds for execution in parallel
 *
 * Makes extensive use of JAGS internals and is potentially unsafe on Windows
 * if an exception is thrown.
 */

#include <string>
#include <algorithm>
#include <vector>
#include <list>

#include <rng/RNGFactory.h>
#include <rng/RNG.h>
#include <model/Model.h>

using std::string;
using std::pair;
using std::vector;
using std::list;

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

//Cut and pasted from jags.cc

static int intArg(SEXP arg)
{
    if (!isNumeric(arg)) {
	error("Invalid integer parameter");
    }
    
    SEXP intarg;
    PROTECT(intarg = AS_INTEGER(arg));
    int i = INTEGER(intarg)[0];
    UNPROTECT(1); //intarg
    return i;
}

static char const *stringArg(SEXP arg, unsigned int i = 0)
{
    if (!isString(arg)) {
	error("Invalid string parameter");
    }
    return R_CHAR(STRING_ELT(arg,i));
}

extern "C" {

    SEXP parallel_seeds(SEXP fac, SEXP n)
    {
	unsigned int nchain = intArg(n);
	string factory = stringArg(fac);
	
	vector<RNG*> rngvec;
	list<pair<RNGFactory*,bool> > const &flist = Model::rngFactories();
	for (list<pair<RNGFactory*, bool> >::const_iterator p = flist.begin(); 
	     p != flist.end(); ++p) 
	{
	    if (p->first->name() == factory) {
		if (p->second) {
		    rngvec = p->first->makeRNGs(nchain);
		    break;
		}
		else {
		    string msg = string("RNG factory not active: ") + factory;
		    error(msg.c_str());
		}
	    }
	}
	
	if (rngvec.empty()) {
	    string msg =  string("RNG factory not found: ") + factory;
	    error(msg.c_str());
	}

	SEXP rng_list;
	PROTECT(rng_list = allocVector(VECSXP, rngvec.size()));

	SEXP elt_names;
	PROTECT(elt_names = allocVector(STRSXP, 2));
	SET_STRING_ELT(elt_names, 0, mkChar(".RNG.name"));
	SET_STRING_ELT(elt_names, 1, mkChar(".RNG.state"));

	for (unsigned int i = 0; i < rngvec.size(); ++i) {

	    SEXP rngname;
	    PROTECT(rngname = mkString(rngvec[i]->name().c_str()));

	    SEXP rngstate;
	    vector<int> istate;
	    rngvec[i]->getState(istate);
	    PROTECT(rngstate = allocVector(INTSXP, istate.size()));
	    for (unsigned int j = 0; j < istate.size(); ++j) {
		INTEGER(rngstate)[j] = istate[j];
	    }

	    SEXP rng_i;
	    PROTECT(rng_i = allocVector(VECSXP, 2));
	    SET_ELEMENT(rng_i, 0, rngname);
	    SET_ELEMENT(rng_i, 1, rngstate);
	    UNPROTECT(2); //istate, rngstate

	    setAttrib(rng_i, R_NamesSymbol, elt_names);

	    SET_ELEMENT(rng_list, i, rng_i);
	    UNPROTECT(1); //rng_i;
	}

	UNPROTECT(2); //elt_names, rng_list
	return rng_list;
    }

}
