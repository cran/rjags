/*
 *  R package rjags file src/jags.cc 
 *  Copyright (C) 2006-2016 Martyn Plummer
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

#include <map>
#include <string>
#include <sstream>
#include <algorithm>
#include <vector>
#include <stdexcept>
#include <cstdio>

#include <Console.h>
#include <version.h>
#include <util/nainf.h>
#include <sarray/SimpleRange.h>

using std::string;
using std::map;
using std::pair;
using std::vector;
using std::copy;
using std::fopen;
using std::fclose;
using std::FILE;

using jags::SArray;
using jags::Console;
using jags::SimpleRange;
using jags::DUMP_DATA;
using jags::DUMP_PARAMETERS;
using jags::FactoryType;
using jags::SAMPLER_FACTORY;
using jags::MONITOR_FACTORY;
using jags::RNG_FACTORY;
using jags::RNG;

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include "init.h"

std::ostringstream jags_out; //Output stream
std::ostringstream jags_err; //Error stream

static bool quiet=false; //Suppress information messages

static void checkConsole (SEXP s)
{
    static SEXP JAGS_console_tag = Rf_install("JAGS_CONSOLE_TAG");
    
    if (TYPEOF(s) != EXTPTRSXP || R_ExternalPtrTag(s) != JAGS_console_tag)
    {
        Rf_error("bad JAGS console pointer");
    }
}

static int intArg(SEXP arg)
{
    if (!Rf_isNumeric(arg)) {
	Rf_error("Invalid integer parameter");
    }
    
    SEXP intarg;
    PROTECT(intarg = AS_INTEGER(arg));
    int i = INTEGER(intarg)[0];
    UNPROTECT(1); //intarg
    return i;
}

static char const *stringArg(SEXP arg, unsigned int i = 0)
{
    if (!Rf_isString(arg)) {
	Rf_error("Invalid string parameter");
    }
    return R_CHAR(STRING_ELT(arg,i));
}

static bool boolArg(SEXP arg)
{
    if (!Rf_isLogical(arg)) {
	Rf_error("Invalid logical parameter");
    }
    return LOGICAL_POINTER(arg)[0];
}

static Console * ptrArg(SEXP ptr)
{
    checkConsole(ptr);
    Console *console = static_cast<Console*>(R_ExternalPtrAddr(ptr));
    if (console == NULL)
	Rf_error("JAGS model must be recompiled");
    return console;
}

static void printMessages(bool status)
{
    /* Print any messages from JAGS and clear the stream buffer */
    if(!jags_out.str().empty()) {
	if (!quiet) Rprintf("%s\n", jags_out.str().c_str());
	jags_out.str("");
    }
    string msg;
    if (!jags_err.str().empty()) {
	msg = jags_err.str();
	jags_err.str("");
    }
    if (status == true) {
	if (!msg.empty()) {
	    Rf_warning("%s\n", msg.c_str());
	}
    }
    else {
	//Something bad happened
	if (!msg.empty()) {
	    Rf_error("%s\n", msg.c_str());
	}
	else {
	    Rf_error("Internal error in JAGS library");
	}
    }
}

static void setSArrayValue(SArray &sarray, SEXP e)
{
    vector<double> v(Rf_length(e));
    copy(NUMERIC_POINTER(e), NUMERIC_POINTER(e) + Rf_length(e), v.begin());
    //Replace R missing values with JAGS missing values
    for (vector<double>::iterator p = v.begin(); p != v.end(); ++p) {
	if (ISNA(*p)) {
	    *p = JAGS_NA;
	}
    }
    sarray.setValue(v);
}

/* Write data from an R list into a JAGS data table */
static void writeDataTable(SEXP data, map<string,SArray> &table)
{
    SEXP names = Rf_getAttrib(data, R_NamesSymbol);
    if (!Rf_isNewList(data)) {
	Rf_error("data must be a list");
    }
    if (Rf_length(names) != Rf_length(data)) {
	Rf_error("data must be a named list");
    }

    for (int i = 0; i < Rf_length(data); ++i) {
	SEXP e = AS_NUMERIC(VECTOR_ELT(data, i));
	if (Rf_length(e) > 0) {
	    string ename = CHAR(STRING_ELT(names, i));
	    SEXP dim = Rf_getAttrib(VECTOR_ELT(data, i), R_DimSymbol); 
	    if (dim == R_NilValue) {
		// Scalar or vector entry.
		SArray sarray(vector<unsigned int>(1, Rf_length(e)));
		setSArrayValue(sarray, e);
		table.insert(pair<string,SArray>(ename, sarray));
	    }
	    else {
		// Array entry
		int ndim = Rf_length(dim);
		vector<unsigned int> idim(ndim);
		for (int j = 0; j < ndim; ++j) {
		    idim[j] = INTEGER(dim)[j];
		}
		SArray sarray(idim);
		setSArrayValue(sarray, e);
		table.insert(pair<string,SArray>(ename, sarray));
	    }
	}
    }
}

static SimpleRange makeRange(SEXP lower, SEXP upper)
{
    if (lower == R_NilValue || upper == R_NilValue) {
	return SimpleRange();
    }
    if (Rf_length(lower) != Rf_length(upper)) {
	Rf_error("Rf_length mismatch between lower and upper limits");
    }
    int n = Rf_length(lower);

    SEXP il, iu;
    PROTECT(il = AS_INTEGER(lower));
    PROTECT(iu = AS_INTEGER(upper));
    vector<int> lvec(n), uvec(n);
    copy(INTEGER(il), INTEGER(il) + n, lvec.begin());
    copy(INTEGER(iu), INTEGER(iu) + n, uvec.begin());
    UNPROTECT(2); //il, iu

    SimpleRange r;
    try {
	r = SimpleRange(lvec, uvec);
    }
    catch (std::logic_error const &except) {                                   
	Rf_error("Invalid range");
    }
    return r;
}

/* Read data from a JAGS data table into an R list */
static SEXP readDataTable(map<string,SArray> const &table)
{
    int N = table.size();

    SEXP data;
    PROTECT(data = Rf_allocVector(VECSXP, N));

    int i;
    map<string,SArray>::const_iterator p;

    for (i = 0, p = table.begin(); p != table.end(); ++p, ++i) {
	int len = p->second.length();

	//Allocate new numeric vector
	SEXP e;
	PROTECT(e = Rf_allocVector(REALSXP, len));

	//Copy values
	vector<double> const &value = p->second.value();
	for (int j = 0; j < len; ++j) {
            if (value[j] == JAGS_NA) {
               NUMERIC_POINTER(e)[j] = NA_REAL;
            }
            else {
	       NUMERIC_POINTER(e)[j] = value[j];
            }
	}
    
	if (p->second.ndim(false) > 1) {

	    //Set dim attribute
	    vector<unsigned int> const &idim = p->second.dim(false);
	    unsigned int ndim = idim.size();
	    SEXP dim;
	    PROTECT(dim = Rf_allocVector(INTSXP, ndim));
	    for (unsigned int k = 0; k < ndim; ++k) {
		INTEGER(dim)[k] = idim[k];
	    }

	    //Set names of the dimensions 
	    vector<string> const &names = p->second.dimNames();
	    if (!names.empty()) {
		SEXP dimnames;
		PROTECT(dimnames = Rf_allocVector(STRSXP, ndim));
		for (unsigned int k = 0; k < ndim; ++k) {
		    SET_STRING_ELT(dimnames, k, Rf_mkChar(names[k].c_str()));
		}
		Rf_setAttrib(dim, R_NamesSymbol, dimnames);
		UNPROTECT(1); //dimnames
	    }
	    SET_DIM(e, dim);
	    UNPROTECT(1); //dim

	    //Set S dimnames
	    bool set_s_dimnames = false;
	    for (unsigned int k = 0; k < ndim; ++k) {
		if (!p->second.getSDimNames(k).empty()) {
		    set_s_dimnames = true;
		    break;
		}
	    }
	    if (set_s_dimnames) {
		SEXP sdimnames;
		PROTECT(sdimnames = Rf_allocVector(VECSXP, ndim));
		for (unsigned int k = 0; k < ndim; ++k) {
		    vector<string> const &names_k = p->second.getSDimNames(k);
		    if (names_k.empty()) {
			SET_VECTOR_ELT(sdimnames, k, R_NilValue);
		    }
		    else {
			SEXP snames_k;
			PROTECT(snames_k = Rf_allocVector(STRSXP, names_k.size()));
			for (unsigned int l = 0; l < names_k.size(); ++l) {
			    SET_STRING_ELT(sdimnames, l, 
					   Rf_mkChar(names_k[l].c_str()));
			}
			UNPROTECT(1); //snames_k
		    }
		}
		Rf_setAttrib(e, R_DimNamesSymbol, sdimnames);
		UNPROTECT(1); //sdimnames
	    }
	}
	else if (!p->second.getSDimNames(0).empty()) {

	    //Set names attribute
	    SEXP snames;
	    vector<string> const &names = p->second.getSDimNames(0);
	    PROTECT(snames = Rf_allocVector(STRSXP, names.size()));
	    for (unsigned int l = 0; l < names.size(); ++l) {
		SET_STRING_ELT(snames, l,  Rf_mkChar(names[l].c_str()));
	    }
	    Rf_setAttrib(e, R_NamesSymbol, snames);
	    UNPROTECT(1); //snames
	}
	    
	SET_ELEMENT(data, i, e);
	UNPROTECT(1); //e
    }

    //Set names
    SEXP names;
    PROTECT(names = Rf_allocVector(STRSXP, table.size()));
    for (i = 0, p = table.begin() ; p != table.end(); ++p, ++i) {
	SET_STRING_ELT(names, i, Rf_mkChar(p->first.c_str()));
    }
    Rf_setAttrib(data, R_NamesSymbol, names);
    UNPROTECT(2); //names, data
    return data;
}

static FactoryType asFactoryType(SEXP type)
{
    string ft = stringArg(type);
    FactoryType ans = SAMPLER_FACTORY;
    if (ft == "sampler") {
	ans = SAMPLER_FACTORY;
    }
    else if (ft == "rng") {
	ans = RNG_FACTORY;
    }
    else if (ft == "monitor") {
	ans = MONITOR_FACTORY;
    }
    else {
	Rf_error("Invalid factory type");
    }
    return ans;
}

extern "C" {

    SEXP quietMessages(SEXP s)
    {
	quiet = boolArg(s);
	return R_NilValue;
    }

    void R_unload_rjags(DllInfo *info)
    {
	//FIXME: Need to zero console pointers

	vector<string> loaded_modules = Console::listModules();
	for (vector<string>::reverse_iterator p = loaded_modules.rbegin();
	     p != loaded_modules.rend(); ++p)
	{
	    Console::unloadModule(*p);
	}
    }

    SEXP clear_console(SEXP s)
    {
	/* Finalizer for console pointers. Frees the external memory
	   and zeroes the pointer when the R object is deleted */

	checkConsole(s);
	Console *console = static_cast<Console*>(R_ExternalPtrAddr(s));
	if (console != NULL) {
	    delete console;
	    R_ClearExternalPtr(s);
	}
	return R_NilValue;
    }

    SEXP make_console()
    {
	void *p = static_cast<void*>(new Console(jags_out, jags_err));
	SEXP ptr = R_MakeExternalPtr(p, Rf_install("JAGS_CONSOLE_TAG"),
				     R_NilValue);
	R_RegisterCFinalizer(ptr, (R_CFinalizer_t) clear_console);
	return ptr;
    }
  
    SEXP check_model(SEXP ptr, SEXP name)
    {
	/* Name should be a name of a file containing the model */
    
	string sname = R_ExpandFileName(stringArg(name));

	FILE *file = fopen(sname.c_str(), "r");
	if (!file) {
	    jags_err << "Failed to open file " << sname << "\n";
	    return R_NilValue;
	}
	else {
	    bool status = ptrArg(ptr)->checkModel(file);	    
 	    printMessages(status);
	    fclose(file);
	    return R_NilValue;
	}
    }

    SEXP compileR(SEXP ptr, SEXP data, SEXP nchain, SEXP gendata)
    {
	if (!Rf_isNumeric(nchain)) {
	    Rf_error("nchain must be numeric");
	}
	if (!Rf_isVector(data)) {
	    Rf_error("invalid data");
	}

	map<string, SArray> table;
	writeDataTable(data, table);
	bool status = ptrArg(ptr)->compile(table, intArg(nchain),
					   boolArg(gendata));
	printMessages(status);
	return R_NilValue;
    }

    SEXP set_parameters(SEXP ptr, SEXP data, SEXP nchain)
    {
	map<string,SArray> data_table;
	writeDataTable(data, data_table);
	bool status = ptrArg(ptr)->setParameters(data_table, intArg(nchain));
	printMessages(status);
	return R_NilValue;
    }
  
    SEXP set_rng_name(SEXP ptr, SEXP name, SEXP chain)
    {
	bool status = ptrArg(ptr)->setRNGname(stringArg(name), intArg(chain));
	printMessages(status);
 	return R_NilValue;
    }
  
    SEXP initialize(SEXP ptr)
    {
	bool status = ptrArg(ptr)->initialize();
	printMessages(status);
	return R_NilValue;
    }

    SEXP check_adaptation(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	bool status = true;
	console->checkAdaptation(status);
	return Rf_ScalarLogical(status);
    }

    SEXP is_adapting(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	return Rf_ScalarLogical(console->isAdapting());
    }

    SEXP adapt_off(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	console->adaptOff();
	return R_NilValue;
    }

    SEXP update(SEXP ptr, SEXP rniter)
    {
        int niter = intArg(rniter);
        Console *console = ptrArg(ptr);
	if (!console->update(niter)) {
	    Rprintf("\n");
	    printMessages(false);
	}
	return R_NilValue;
    }

    SEXP set_monitors(SEXP ptr, SEXP names, SEXP lower, SEXP upper, 
		      SEXP thin, SEXP type)
    {
	if (!Rf_isString(names)) {
	    Rf_error("names must be a character vector");
	}

	int n = Rf_length(names);
	if (Rf_length(lower) != n || Rf_length(upper) != n) {
	    Rf_error("length of names must match length of lower and upper");
	}

	SEXP status; //Was attempt to set monitor successful?
	PROTECT(status = Rf_allocVector(LGLSXP, n));
	for (int i = 0; i < n; ++i) {
	    SimpleRange range = makeRange(VECTOR_ELT(lower, i), 
					  VECTOR_ELT(upper, i));
	    bool ok = ptrArg(ptr)->setMonitor(stringArg(names,i), range, 
					      intArg(thin), 
					      stringArg(type));
	    printMessages(true);
	    LOGICAL_POINTER(status)[i] = ok;
	}
	UNPROTECT(1); //status
	return status;
    }

    SEXP clear_monitor(SEXP ptr, SEXP name, SEXP lower, SEXP upper, SEXP type)
    {
        SimpleRange range = makeRange(lower, upper);
	bool status = ptrArg(ptr)->clearMonitor(stringArg(name), range, 
						stringArg(type));
	printMessages(status);
	return R_NilValue;
    }

    SEXP get_monitored_values(SEXP ptr, SEXP type)
    {
	map<string,SArray> data_table;
	bool status = ptrArg(ptr)->dumpMonitors(data_table, stringArg(type),
						false);
	printMessages(status);
	return readDataTable(data_table);
    }

    //FIXME: lazy cut-and-paste here
    SEXP get_monitored_values_flat(SEXP ptr, SEXP type)
    {
	map<string,SArray> data_table;
	bool status = ptrArg(ptr)->dumpMonitors(data_table, stringArg(type),
						true);
	printMessages(status);
	return readDataTable(data_table);
    }

    SEXP get_data(SEXP ptr)
    {
	map<string,SArray> data_table;
	string rngname; //Not actually needed
	bool status = ptrArg(ptr)->dumpState(data_table, rngname, DUMP_DATA, 1);
	printMessages(status);
	return readDataTable(data_table);
    }

    SEXP get_state(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	unsigned int nchain = console->nchain();
	if (nchain == 0) {
	    return R_NilValue;
	}

	//ans is the list that contains the state for each chain
	SEXP ans;
	PROTECT(ans = Rf_allocVector(VECSXP, nchain));
	for (unsigned int n = 0; n < nchain; ++n) {
	    string srng;
	    map<string,SArray> param_table;
	    console->dumpState(param_table, srng, DUMP_PARAMETERS, n+1);
	    //Read the parameter values into an R list
	    SEXP params;
	    PROTECT(params = readDataTable(param_table));
	    int nparam = Rf_length(params);
	    SEXP names = Rf_getAttrib(params, R_NamesSymbol);
	    //Now we have to make a copy of the list with an extra element
	    SEXP staten, namesn;
	    PROTECT(staten = Rf_allocVector(VECSXP, nparam + 1));
	    PROTECT(namesn = Rf_allocVector(STRSXP, nparam + 1));
	    for (int j = 0; j < nparam; ++j) {
		SET_ELEMENT(staten, j, VECTOR_ELT(params, j));
		SET_STRING_ELT(namesn, j, STRING_ELT(names, j));
	    }
	    //Assign .RNG.name as the last element
	    SEXP rngname;
	    PROTECT(rngname = Rf_mkString(srng.c_str()));
	    SET_ELEMENT(staten, nparam, rngname);
	    SET_STRING_ELT(namesn, nparam, Rf_mkChar(".RNG.name"));
	    Rf_setAttrib(staten, R_NamesSymbol, namesn);
	    //And we're done with this chain
	    SET_ELEMENT(ans, n, staten);
	    UNPROTECT(4); //rngname, namesn, staten, params
	}
	UNPROTECT(1); //ans
	return ans;
    }


    SEXP get_variable_names(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	vector<string> const &namevec = console->variableNames();
	SEXP varnames;
	PROTECT(varnames = Rf_allocVector(STRSXP,namevec.size()));
	for (unsigned int i = 0; i < namevec.size(); ++i) {
	    SET_STRING_ELT(varnames, i, Rf_mkChar(namevec[i].c_str()));
	}
	UNPROTECT(1); //varnames
	return varnames;
    }

    SEXP get_samplers(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	vector<vector<string> > samplers;
	bool status = console->dumpSamplers(samplers);
	printMessages(status);
	    
	unsigned int n = samplers.size();
	SEXP node_list, sampler_names;
	PROTECT(node_list = Rf_allocVector(VECSXP, n));
	PROTECT(sampler_names = Rf_allocVector(STRSXP, n));
	
	for (unsigned int i = 0; i < n; ++i) {
	    int nnode = samplers[i].size() - 1;
	    SEXP e;
	    PROTECT(e=Rf_allocVector(STRSXP, nnode));
	    for (int j = 0; j < nnode; ++j) {
		SET_STRING_ELT(e, j, Rf_mkChar(samplers[i][j+1].c_str()));
	    }
	    SET_ELEMENT(node_list, i, e);
	    SET_STRING_ELT(sampler_names, i, Rf_mkChar(samplers[i][0].c_str()));
	    UNPROTECT(1); //e
	}
	Rf_setAttrib(node_list, R_NamesSymbol, sampler_names);	
	UNPROTECT(2); //node_list, sampler_names
	return node_list;
    }

    SEXP get_factories(SEXP type)
    {
	FactoryType ft = asFactoryType(type);
	vector<pair<string, bool> > factories = Console::listFactories(ft);
	unsigned int n = factories.size();

	SEXP fac_list;
	PROTECT(fac_list = Rf_allocVector(VECSXP, 2));

	SEXP names, status;
	PROTECT(names = Rf_allocVector(STRSXP, n));
	PROTECT(status = Rf_allocVector(LGLSXP, n));
	for (unsigned int i = 0; i < n; ++i) {
	    SET_STRING_ELT(names, i, Rf_mkChar(factories[i].first.c_str()));
	    LOGICAL_POINTER(status)[i] = factories[i].second;
	}

	SET_ELEMENT(fac_list, 0, names);
	SET_ELEMENT(fac_list, 1, status);
	UNPROTECT(2); //names, status

	SEXP fac_names;
	PROTECT(fac_names = Rf_allocVector(STRSXP,2));
	SET_STRING_ELT(fac_names, 0, Rf_mkChar("factory"));
	SET_STRING_ELT(fac_names, 1, Rf_mkChar("status"));
	Rf_setAttrib(fac_list, R_NamesSymbol, fac_names);	
	UNPROTECT(1); //fac_names

	UNPROTECT(1); //fac_list
	return fac_list;
    }

    SEXP set_factory_active(SEXP name, SEXP type, SEXP status)
    {
	Console::setFactoryActive(stringArg(name), asFactoryType(type), 
				  boolArg(status));
	return R_NilValue;
    }

    SEXP get_iter(SEXP ptr)
    {
	return Rf_ScalarInteger(ptrArg(ptr)->iter());
    }
    
    SEXP get_nchain(SEXP ptr)
    {
	return Rf_ScalarInteger(ptrArg(ptr)->nchain());
    }

    SEXP load_module(SEXP name)
    {
	return Rf_ScalarLogical(Console::loadModule(stringArg(name)));
    }

    SEXP unload_module(SEXP name)
    {
	return Rf_ScalarLogical(Console::unloadModule(stringArg(name)));
    }

    SEXP get_modules()
    {
	vector<string> modules = Console::listModules();
	unsigned int n = modules.size();
	SEXP mod_list;
	PROTECT(mod_list = Rf_allocVector(STRSXP, n));
	for (unsigned int i = 0; i < n; ++i) {
	    SET_STRING_ELT(mod_list, i, Rf_mkChar(modules[i].c_str()));
	}
	UNPROTECT(1); //mod_list
	return mod_list;
    }
    
    SEXP get_version()
    {
	return Rf_mkString(jags_version());
    }

}
