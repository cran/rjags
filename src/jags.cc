#include <map>
#include <string>
#include <sstream>
#include <algorithm>
#include <vector>
#include <stdexcept>

#include <Console.h>
#include <version.h>
#include <util/nainf.h>

#include <R.h>

using std::string;
using std::map;
using std::pair;
using std::vector;
using std::copy;

/* Workaround length being remapped to Rf_length
   by the preprocessor */

unsigned long sarray_len(SArray const &s)
{
  return s.length();
}

int min2(int a, int b)
{
  return std::min(a,b);
}

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

std::ostringstream jags_out; //Output stream
std::ostringstream jags_err; //Error stream
static SEXP JAGS_console_tag; //Run-time type checking for external pointer
static bool quiet=false; //Suppress information messages

static void checkConsole (SEXP s)
{				  
    if (TYPEOF(s) != EXTPTRSXP || R_ExternalPtrTag(s) != JAGS_console_tag)
    {
        error("bad JAGS console pointer");
    }
}

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

static bool boolArg(SEXP arg)
{
    if (!isLogical(arg)) {
	error("Invalid logical parameter");
    }
    return LOGICAL_POINTER(arg)[0];
}

static Console * ptrArg(SEXP ptr)
{
    checkConsole(ptr);
    Console *console = static_cast<Console*>(R_ExternalPtrAddr(ptr));
    if (console == NULL)
	error("JAGS model must be recompiled");
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
	    warning("%s\n", msg.c_str());
	}
    }
    else {
	//Something bad happened
	if (!msg.empty()) {
	    error("%s\n", msg.c_str());
	}
	else {
	    error("Internal error in JAGS library");
	}
    }
}

static void setSArrayValue(SArray &sarray, SEXP e)
{
    vector<double> v(length(e));
    copy(NUMERIC_POINTER(e), NUMERIC_POINTER(e) + length(e), v.begin());
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
    SEXP names = getAttrib(data, R_NamesSymbol);
    if (!isNewList(data)) {
	error("data must be a list");
    }
    if (length(names) != length(data)) {
	error("data must be a named list");
    }

    for (int i = 0; i < length(data); ++i) {
	SEXP e = AS_NUMERIC(VECTOR_ELT(data, i));
	if (length(e) > 0) {
	    string ename = CHAR(STRING_ELT(names, i));
	    SEXP dim = getAttrib(VECTOR_ELT(data, i), R_DimSymbol); 
	    if (dim == R_NilValue) {
		// Scalar or vector entry.
		SArray sarray(vector<unsigned int>(1, length(e)));
		setSArrayValue(sarray, e);
		table.insert(pair<string,SArray>(ename, sarray));
	    }
	    else {
		// Array entry
		int ndim = length(dim);
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

static Range makeRange(SEXP lower, SEXP upper)
{
    if (lower == R_NilValue || upper == R_NilValue) {
	return Range();
    }
    if (length(lower) != length(upper)) {
	error("length mismatch between lower and upper limits");
    }
    int n = length(lower);

    SEXP il, iu;
    PROTECT(il = AS_INTEGER(lower));
    PROTECT(iu = AS_INTEGER(upper));
    vector<int> lvec(n), uvec(n);
    copy(INTEGER(il), INTEGER(il) + n, lvec.begin());
    copy(INTEGER(iu), INTEGER(iu) + n, uvec.begin());
    UNPROTECT(2); //il, iu

    Range r;
    try {
	r = Range(lvec, uvec);
    }
    catch (std::logic_error except) {                                   
	error("Invalid range");
    }
    return r;
}

/* Read data from a JAGS data table into an R list */
static SEXP readDataTable(map<string,SArray> const &table)
{
    int N = table.size();

    SEXP data;
    PROTECT(data = allocVector(VECSXP, N));

    int i;
    map<string,SArray>::const_iterator p;

    for (i = 0, p = table.begin(); p != table.end(); ++p, ++i) {
	int len = sarray_len(p->second);

	//Allocate new numeric vector
	SEXP e;
	PROTECT(e = allocVector(REALSXP, len));

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
	    PROTECT(dim = allocVector(INTSXP, ndim));
	    for (unsigned int k = 0; k < ndim; ++k) {
		INTEGER(dim)[k] = idim[k];
	    }

	    //Set names of the dimensions 
	    vector<string> const &names = p->second.dimNames();
	    if (!names.empty()) {
		SEXP dimnames;
		PROTECT(dimnames = allocVector(STRSXP, ndim));
		for (unsigned int k = 0; k < ndim; ++k) {
		    SET_STRING_ELT(dimnames, k, mkChar(names[k].c_str()));
		}
		setAttrib(dim, R_NamesSymbol, dimnames);
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
		PROTECT(sdimnames = allocVector(VECSXP, ndim));
		for (unsigned int k = 0; k < ndim; ++k) {
		    vector<string> const &names_k = p->second.getSDimNames(k);
		    if (names_k.empty()) {
			SET_VECTOR_ELT(sdimnames, k, R_NilValue);
		    }
		    else {
			SEXP snames_k;
			PROTECT(snames_k = allocVector(STRSXP, names_k.size()));
			for (unsigned int l = 0; l < names_k.size(); ++l) {
			    SET_STRING_ELT(sdimnames, l, 
					   mkChar(names_k[l].c_str()));
			}
			UNPROTECT(1); //snames_k
		    }
		}
		setAttrib(e, R_DimNamesSymbol, sdimnames);
		UNPROTECT(1); //sdimnames
	    }
	}
	else if (!p->second.getSDimNames(0).empty()) {

	    //Set names attribute
	    SEXP snames;
	    vector<string> const &names = p->second.getSDimNames(0);
	    PROTECT(snames = allocVector(STRSXP, names.size()));
	    for (unsigned int l = 0; l < names.size(); ++l) {
		SET_STRING_ELT(snames, l,  mkChar(names[l].c_str()));
	    }
	    setAttrib(e, R_NamesSymbol, snames);
	    UNPROTECT(1); //snames
	}
	    
	SET_ELEMENT(data, i, e);
	UNPROTECT(1); //e
    }

    //Set names
    SEXP names;
    PROTECT(names = allocVector(STRSXP, table.size()));
    for (i = 0, p = table.begin() ; p != table.end(); ++p, ++i) {
	SET_STRING_ELT(names, i, mkChar(p->first.c_str()));
    }
    setAttrib(data, R_NamesSymbol, names);
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
	error("Invalid factory type");
    }
    return ans;
}

extern "C" {

    void quietMessages(SEXP s)
    {
	quiet = boolArg(s);
    }

    void R_init_rjags(DllInfo *info)
    {
	JAGS_console_tag = install("JAGS_CONSOLE_TAG");	
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
	SEXP ptr = R_MakeExternalPtr(p, JAGS_console_tag, R_NilValue);
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

    SEXP compile(SEXP ptr, SEXP data, SEXP nchain, SEXP gendata)
    {
	if (!isNumeric(nchain)) {
	    error("nchain must be numeric");
	}
	if (!isVector(data)) {
	    error("invalid data");
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
	return ScalarLogical(status);
    }

    SEXP is_adapting(SEXP ptr)
    {
	Console *console = ptrArg(ptr);
	return ScalarLogical(console->isAdapting());
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
	if (!isString(names)) {
	    error("names must be a character vector");
	}

	int n = length(names);
	if (length(lower) != n || length(upper) != n) {
	    error("length of names must match length of lower and upper");
	}

	SEXP status; //Was attempt to set monitor successful?
	PROTECT(status = allocVector(LGLSXP, n));
	for (int i = 0; i < n; ++i) {
	    Range range = makeRange(VECTOR_ELT(lower, i), VECTOR_ELT(upper, i));
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
        Range range = makeRange(lower, upper);
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
	PROTECT(ans = allocVector(VECSXP, nchain));
	for (unsigned int n = 0; n < nchain; ++n) {
	    string srng;
	    map<string,SArray> param_table;
	    console->dumpState(param_table, srng, DUMP_PARAMETERS, n+1);
	    //Read the parameter values into an R list
	    SEXP params;
	    PROTECT(params = readDataTable(param_table));
	    int nparam = length(params);
	    SEXP names = getAttrib(params, R_NamesSymbol);
	    //Now we have to make a copy of the list with an extra element
	    SEXP staten, namesn;
	    PROTECT(staten = allocVector(VECSXP, nparam + 1));
	    PROTECT(namesn = allocVector(STRSXP, nparam + 1));
	    for (int j = 0; j < nparam; ++j) {
		SET_ELEMENT(staten, j, VECTOR_ELT(params, j));
		SET_STRING_ELT(namesn, j, STRING_ELT(names, j));
	    }
	    //Assign .RNG.name as the last element
	    SEXP rngname;
	    PROTECT(rngname = mkString(srng.c_str()));
	    SET_ELEMENT(staten, nparam, rngname);
	    SET_STRING_ELT(namesn, nparam, mkChar(".RNG.name"));
	    setAttrib(staten, R_NamesSymbol, namesn);
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
	PROTECT(varnames = allocVector(STRSXP,namevec.size()));
	for (unsigned int i = 0; i < namevec.size(); ++i) {
	    SET_STRING_ELT(varnames, i, mkChar(namevec[i].c_str()));
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
	PROTECT(node_list = allocVector(VECSXP, n));
	PROTECT(sampler_names = allocVector(STRSXP, n));
	
	for (unsigned int i = 0; i < n; ++i) {
	    int nnode = samplers[i].size() - 1;
	    SEXP e;
	    PROTECT(e=allocVector(STRSXP, nnode));
	    for (int j = 0; j < nnode; ++j) {
		SET_STRING_ELT(e, j, mkChar(samplers[i][j+1].c_str()));
	    }
	    SET_ELEMENT(node_list, i, e);
	    SET_STRING_ELT(sampler_names, i, mkChar(samplers[i][0].c_str()));
	    UNPROTECT(1); //e
	}
	setAttrib(node_list, R_NamesSymbol, sampler_names);	
	UNPROTECT(2); //node_list, sampler_names
	return node_list;
    }

    SEXP get_factories(SEXP type)
    {
	FactoryType ft = asFactoryType(type);
	vector<pair<string, bool> > factories = Console::listFactories(ft);
	unsigned int n = factories.size();

	SEXP fac_list;
	PROTECT(fac_list = allocVector(VECSXP, 2));

	SEXP names, status;
	PROTECT(names = allocVector(STRSXP, n));
	PROTECT(status = allocVector(LGLSXP, n));
	for (unsigned int i = 0; i < n; ++i) {
	    SET_STRING_ELT(names, i, mkChar(factories[i].first.c_str()));
	    LOGICAL_POINTER(status)[i] = factories[i].second;
	}

	SET_ELEMENT(fac_list, 0, names);
	SET_ELEMENT(fac_list, 1, status);
	UNPROTECT(2); //names, status

	SEXP fac_names;
	PROTECT(fac_names = allocVector(STRSXP,2));
	SET_STRING_ELT(fac_names, 0, mkChar("factory"));
	SET_STRING_ELT(fac_names, 1, mkChar("status"));
	setAttrib(fac_list, R_NamesSymbol, fac_names);	
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
	return ScalarInteger(ptrArg(ptr)->iter());
    }
    
    SEXP get_nchain(SEXP ptr)
    {
	return ScalarInteger(ptrArg(ptr)->nchain());
    }

    SEXP load_module(SEXP name)
    {
	return ScalarLogical(Console::loadModule(stringArg(name)));
    }

    SEXP unload_module(SEXP name)
    {
	return ScalarLogical(Console::unloadModule(stringArg(name)));
    }

    SEXP get_modules()
    {
	vector<string> modules = Console::listModules();
	unsigned int n = modules.size();
	SEXP mod_list;
	PROTECT(mod_list = allocVector(STRSXP, n));
	for (unsigned int i = 0; i < n; ++i) {
	    SET_STRING_ELT(mod_list, i, mkChar(modules[i].c_str()));
	}
	UNPROTECT(1); //mod_list
	return mod_list;
    }
    
    SEXP get_version()
    {
	return mkString(jags_version());
    }

}
