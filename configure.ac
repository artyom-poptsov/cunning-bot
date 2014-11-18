dnl   configuration script for Cunning-Bot
dnl   Process this file with autoconf to produce configure.
dnl

AC_INIT([cunning-bot], [0.0], [aidalgol@amuri.net],
                       [cunning-bot],
                       [https://github.com/aidalgol/cunning-bot])

AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE

AC_ARG_WITH([guilesitedir],
            [AS_HELP_STRING([--with-guilesitedir],
                            [use the specified installation path for Guile modules])],
            [case "x$withval" in
             xyes|xno) guilesitedir="";;
             *)        guilesitedir="$withval";;
             esac],
             [guilesitedir=""])

GUILE_PROGS
GUILE_SITE_DIR

GUILE_MODULE_REQUIRED([dsv])

pkgdatadir="$datadir/$PACKAGE"
if test "x$guilesitedir" = "x"; then
   guilesitedir="$pkgdatadir"
fi
AC_SUBST([guilesitedir])

AC_CONFIG_FILES([Makefile plugins/Makefile])

# Generate a Makefile, based on the results.
AC_OUTPUT()

if test "$guilesitedir" != "$GUILE_SITE"; then
   # Guile has a different prefix than this module
   AC_MSG_WARN([]
   [The Guile modules will be installed in ${guilesitedir}.]
   [You should probably re-run `configure' with]
   [`--with-guilesitedir=$GUILE_SITE']
   [Otherwise, you will have to adjust the `GUILE_LOAD_PATH' environment]
   [variable.])
fi
