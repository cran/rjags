AC_INIT([rjags],[4])

if test -z "${R_HOME}"; then
   AC_MSG_ERROR("R_HOME is not defined")
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`

# Use pkg-config to set compiler flags

# Note: pkg-config is not installed on vanilla macOS but is provided by the JAGS installers at /opt/jags/versions/pkgconf-lite/current/bin
AC_PATH_PROG([PKG_CONFIG], [pkg-config], [], [$PATH:/usr/local/bin:/opt/jags/versions/pkgconf-lite/current/bin])
if test [ -n "${PKG_CONFIG}" ] ; then
   # The official macOS installer for JAGS >= 5 should have the jags.pc file for the correct major version of JAGS here:
   PKG_CONFIG_PATH_MACOS="/opt/jags/lib/pkgconfig-${PACKAGE_VERSION}"
   # Add legacy path to workaround issue on r-universe for x86_64 macOS:
   if test [ -z "${PKG_CONFIG_PATH}" ]; then
       export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig"
   else
       export PKG_CONFIG_PATH="${PKG_CONFIG_PATH}:/usr/local/lib/pkgconfig"
   fi
   # Detect a jags.pc in any possible location:
   if `PKG_CONFIG_PATH="${PKG_CONFIG_PATH_MACOS}:${PKG_CONFIG_PATH}" ${PKG_CONFIG} --exists jags`; then
       # Prefer the version on the default search path:
       if `${PKG_CONFIG} --exists --atleast-version=${PACKAGE_VERSION}.0.0 jags` &&
          `${PKG_CONFIG} --exists --max-version=${PACKAGE_VERSION}.99.99 jags`; then
         JAGS_LIBS=`${PKG_CONFIG} --libs jags`
         JAGS_CFLAGS=`${PKG_CONFIG} --cflags jags`
         JAGS_LIBDIR=`${PKG_CONFIG} --variable libdir jags`
         JAGS_ROOT=`${PKG_CONFIG} --variable prefix jags`
         if test -z "${JAGS_LIBS}" || test -z "${JAGS_CFLAGS}" ; then
            AC_MSG_ERROR([Cannot get information about jags ${PACKAGE_VERSION} from pkg-config])
         else
            AC_MSG_NOTICE([Setting compile and link flags according to pkg-config])
            AC_MSG_NOTICE([Compile flags are $JAGS_CFLAGS])
            AC_MSG_NOTICE([Link flags are $JAGS_LIBS])
         fi
       # If the first jags.pc on the search path is for the wrong version, try the macOS official installation directory:
       elif `PKG_CONFIG_PATH="${PKG_CONFIG_PATH_MACOS}" ${PKG_CONFIG} --exists --atleast-version=${PACKAGE_VERSION}.0.0 jags` &&
           `PKG_CONFIG_PATH="${PKG_CONFIG_PATH_MACOS}" ${PKG_CONFIG} --exists --max-version=${PACKAGE_VERSION}.99.99 jags`; then
         JAGS_LIBS=`PKG_CONFIG_PATH="${PKG_CONFIG_PATH_MACOS}" ${PKG_CONFIG} --libs jags`
         JAGS_CFLAGS=`PKG_CONFIG_PATH="${PKG_CONFIG_PATH_MACOS}" ${PKG_CONFIG} --cflags jags`
         JAGS_LIBDIR=`PKG_CONFIG_PATH="${PKG_CONFIG_PATH_MACOS}" ${PKG_CONFIG} --variable libdir jags`
         JAGS_ROOT=`PKG_CONFIG_PATH="${PKG_CONFIG_PATH_MACOS}" ${PKG_CONFIG} --variable prefix jags`
         if test -z "${JAGS_LIBS}" || test -z "${JAGS_CFLAGS}" ; then
            AC_MSG_ERROR([Cannot get information about jags ${PACKAGE_VERSION} from pkg-config]
          [using /opt/jags/lib/pkgconfig-${PACKAGE_VERSION}])
         else
           AC_MSG_NOTICE([Setting compile and link flags according to pkg-config]
           [using /opt/jags/lib/pkgconfig-${PACKAGE_VERSION}])
           AC_MSG_NOTICE([Compile flags are $JAGS_CFLAGS])
           AC_MSG_NOTICE([Link flags are $JAGS_LIBS])
         fi
       else
         if `${PKG_CONFIG} --exists jags`; then
           jags_installed_version=`${PKG_CONFIG} --modversion jags`
           AC_MSG_ERROR([pkg-config found JAGS ${jags_installed_version} but rjags requires JAGS ${PACKAGE_VERSION}.x.y])
         else
           # Should never happen unless we have a broken macOS official JAGS installation - but just to be safe:
           jags_installed_version=`PKG_CONFIG_PATH="${PKG_CONFIG_PATH_MACOS}" ${PKG_CONFIG} --modversion jags`
           AC_MSG_ERROR([pkg-config found JAGS ${jags_installed_version} under /opt/jags/lib/pkgconfig-${PACKAGE_VERSION} but rjags requires JAGS ${PACKAGE_VERSION}.x.y]
           [It seems you have a broken macOS official JAGS installation])
         fi
       fi
   else 
      AC_MSG_ERROR([pkg-config file for jags ${PACKAGE_VERSION} unavailable]
                   [Try adding the directory containing 'jags.pc']
                   [to the PKG_CONFIG_PATH environment variable])
   fi
else
   AC_MSG_ERROR([pkg-config is required to configure rjags])
fi

# Support for private installations of JAGS. If the jags library is not
# on the linker path then it helps to hard-code the location of JAGS into
# the DSO for rjags. Suggested by Doug Bates, April 2011.

# Options passed to the linker are not portable, so the current version is
# Linux only. There is scope for adding other platforms with the right
# incantation.

AC_ARG_ENABLE([rpath],
   AS_HELP_STRING([--enable-rpath],[Hard link the location of the JAGS library (Linux only)]))
if test ["x$enable_rpath" = "xyes"]; then
   host_os=`uname`;
   case "${host_os}" in
      Linux*)
         JAGS_RPATH="-Wl,-rpath=${JAGS_LIBDIR}"
      ;;
   *)
      AC_MSG_ERROR([Option --enable-rpath is not available for OS $host_os])
      ;;
   esac
fi

# Sanity checks. 
OLDFLAGS=${LDFLAGS}
OCXXFLAGS=${CXXFLAGS}
AC_LANG_PUSH(C++)
CXXFLAGS=${JAGS_CFLAGS}
LDFLAGS="-L${JAGS_LIBDIR} ${JAGS_RPATH}"
# Can we link to the library?
AC_CHECK_LIB(jags, jags_version, [],
             [AC_MSG_ERROR("cannot link to JAGS library in ${JAGS_LIBDIR}.")])
# Does the library have the correct major version number?
# Need to run this trivial test program first
AC_MSG_CHECKING([runtime link to JAGS library])
AC_RUN_IFELSE(
   [AC_LANG_SOURCE([
      #include <version.h>
      int main() { return 0; }])],
   [AC_MSG_RESULT([OK])],
   [AC_MSG_RESULT([link error])
    AC_MSG_ERROR([runtime link error: See the INSTALL file for help])],
   [AC_MSG_RESULT([skipped for cross compilation])])
# Now we can test
AC_MSG_CHECKING([version of JAGS library])
AC_RUN_IFELSE(
   [AC_LANG_SOURCE([
      #include <version.h>
      int main() { return JAGS_MAJOR == ${PACKAGE_VERSION} ? 0 : 1; }])],
   [AC_MSG_RESULT([OK])],
   [AC_MSG_RESULT([wrong version])
    AC_MSG_ERROR([rjags requires JAGS version ${PACKAGE_VERSION}.x.y])],
   [AC_MSG_RESULT([skipped for cross compilation])])
AC_LANG_POP(C++)
LDFLAGS=${OLDFLAGS}
CXXFLAGS=${OCXXFLAGS}

# Set location of modules

JAGS_MODDIR=${JAGS_LIBDIR}/JAGS/modules-${PACKAGE_VERSION}

AC_SUBST(JAGS_LIBS)
AC_SUBST(JAGS_CFLAGS)
AC_SUBST(JAGS_MODDIR)
AC_SUBST(JAGS_RPATH)
AC_SUBST(JAGS_ROOT)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
AC_CONFIG_FILES([R/unix/zzz.R])
AC_OUTPUT
