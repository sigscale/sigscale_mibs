## configure.ac
## vim: ts=3
##
## Process this file with autoconf to produce a configure script.
##
## Boilerplate:  standard boilerplate code
##
AC_INIT([sigscale_mibs], [1.1.1], [issues@sigscale.org])
AC_COPYRIGHT([Copyright (c) 2024 SigScale Global Inc.])
AC_PREREQ(2.63)
AC_CONFIG_SRCDIR([src/sigscale_mib.erl])
AC_CONFIG_HEADERS([config.h])
AC_LANG([Erlang])
AM_INIT_AUTOMAKE([-Wno-portability])

##
## Options:  control options to configure, support code for the options
##
AC_ARG_ENABLE([debug],
		[AS_HELP_STRING([--disable-debug], [disable debugging (default is no)])])


##
## Programs:  check for programs needed by the configure process,
##            the build process, or by one of the programs being built
##
AC_ERLANG_NEED_ERLC
if test -z "${ERLCFLAGS}" ; then
	ERLCFLAGS="-W -v"
fi
if test "x$disable_debug" != xyes ; then
	ERLCFLAGS="$ERLCFLAGS +debug_info"
fi
AC_ERLANG_SUBST_LIB_DIR
if test -z "${ERLANG_INSTALL_LIB_DIR}" ; then
	ERLANG_INSTALL_LIB_DIR=$ERLANG_LIB_DIR
fi
AC_ERLANG_SUBST_ROOT_DIR
AC_PROG_MKDIR_P
AC_PROG_INSTALL
AC_PROG_LN_S
AC_ERLANG_SUBST_INSTALL_LIB_DIR
appdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION
erldir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/src
beamdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/ebin
docdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/doc
privdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv
emibdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv/mibs
mibsdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/mibs
reldir=$ERLANG_INSTALL_LIB_DIR/../releases
AC_SUBST(appdir)
AC_SUBST(erldir)
AC_SUBST(beamdir)
AC_SUBST(privdir)
AC_SUBST(emibdir)
AC_SUBST(mibsdir)
AC_SUBST(reldir)


##
## Libraries:  check for libraries
##
AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_CHECK_LIB([kernel], [], [AC_MSG_ERROR([kernel was not found!])])
AC_ERLANG_CHECK_LIB([stdlib], [], [AC_MSG_ERROR([stdlib was not found!])])
AC_ERLANG_CHECK_LIB([sasl], [], [AC_MSG_ERROR([sasl was not found!])])
AM_CONDITIONAL([MAKE_TAR_EXTRA_FILES],
		[AS_VERSION_COMPARE([$ERLANG_LIB_VER_sasl], ["4.0"], [false], [true], [true])])
AC_ERLANG_CHECK_LIB([snmp], [], [AC_MSG_ERROR([snmp was not found!])])


##
## Headers:  check for header files
##


##
## Typedefs & Structures:  check for typedefs, structures,
##                         and compiler characteristics.
##


##
## Functions:  check for library functions
##


##
## Output:  create output files
## 
AC_OUTPUT([Makefile ebin/Makefile doc/Makefile priv/Makefile
		priv/mibs/Makefile mibs/Makefile test/Makefile
		$PACKAGE_NAME.rel ebin/$PACKAGE_NAME.appup sys.config])

