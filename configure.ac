AC_INIT([rjags], [1.0.2])

AC_PREFIX_PROGRAM(jags)

#Set optional include path with option --with-jags-include or
#environment variable JAGS_INCLUDE

AC_ARG_WITH([jags-include],
            AC_HELP_STRING([--with-jags-include=INCLUDE_PATH],
                           [the location of JAGS header files]),
            [jags_include_path=$withval])
if test [ -n "$jags_include_path" ] ; then
   JAGS_INCLUDE=${jags_include_path}
else
   if test [ -z "${JAGS_INCLUDE}" ] ; then
      if test [ "$prefix" == "NONE" ]; then
         AC_MSG_ERROR("JAGS headers not found. Use --with-jags-include configure option")
      else
         JAGS_INCLUDE=${prefix}/include/JAGS
      fi
   fi
fi

#Set optional library path with option --with-jags-lib or
#environment variable JAGS_LIBS

AC_ARG_WITH([jags-lib],
            AC_HELP_STRING([--with-jags-lib=LIB_PATH],
                           [the location of the JAGS library]),
            [jags_lib_path=$withval])
if test [ -n "$jags_lib_path" ] ; then
   JAGS_LIB=$jags_lib_path
else
   if test [ -z "${JAGS_LIB}" ] ; then
      if test [ "${prefix}" == "NONE" ] ; then 
         AC_MSG_ERROR("JAGS library not found. Use --with-jags-lib configure option")
      else
         JAGS_LIB=${prefix}/lib
      fi
   fi
fi

#Finally, set optional module directory using --with-jags-modules or
#environment variable JAGS_MODULES

AC_ARG_WITH([jags-modules],
            AC_HELP_STRING([--with-jags-modules=MOD_PATH],
                           [the location of the JAGS modules]),
            [jags_mod_path=$withval])
if test [ -n "$jags_mod_path" ] ; then
   JAGS_MODULES=$jags_mod_path
else
   if test [ -z "${JAGS_MODULES}" ] ; then
      JAGS_MODULES=${JAGS_LIB}/JAGS/modules
   fi
fi

AC_SUBST(JAGS_INCLUDE)
AC_SUBST(JAGS_LIB)
AC_SUBST(JAGS_MODULES)

AC_OUTPUT(src/Makevars)
AC_OUTPUT(R/unix/zzz.R)
