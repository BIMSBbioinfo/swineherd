dnl SPDX-FileCopyrightText: 2023 Ricardo Wurmus <rekado@elephly.net>
dnl
dnl SPDX-License-Identifier: CC0-1.0

AC_INIT([swineherd],
  [m4_esyscmd([build-aux/git-version-gen .tarball-version])],
  [],
  [swineherd],
  [https://elephly.net/])

AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([gnu silent-rules tar-ustar color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([3.0])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your Guile installation.])
fi
GUILE_MODULE_AVAILABLE([have_shepherd], [(shepherd service)])
if test "x$have_shepherd" != "xyes"; then
  AC_MSG_ERROR([Shepherd is missing; please install it.])
fi
GUILE_MODULE_AVAILABLE([have_fibers], [(fibers web server)])
if test "x$have_fibers" != "xyes"; then
  AC_MSG_ERROR([Guile-Fibers is missing; please install it.])
fi
GUILE_MODULE_AVAILABLE([have_config], [(config api)])
if test "x$have_config" != "xyes"; then
  AC_MSG_ERROR([Guile-Config is missing; please install it.])
fi
GUILE_MODULE_AVAILABLE([have_netlink], [(ip link)])
if test "x$have_netlink" != "xyes"; then
  AC_MSG_ERROR([Guile-Netlink is missing; please install it.])
fi
GUILE_MODULE_AVAILABLE([have_guix], [(gnu build linux-container)])
if test "x$have_guix" != "xyes"; then
  AC_MSG_ERROR([Guix is missing; please install it.])
fi

dnl Check for required programmes and store their full path in the
dnl given variables.  The variables are used to substitute
dnl placeholders in the scripts.

dnl The variables only need to be set manually when they cannot be
dnl found via the PATH environment variable.
AC_DEFUN([find_or_override_prog],
[AC_ARG_VAR($1, override location of $2 executable)dnl
AS_IF([test -z "$$1"],dnl
      [AC_PATH_PROG([$1], [$2], no)dnl
      if test x"$$1" == x"no" ; then
        AC_MSG_ERROR([Could not find $2.])
      fi
      ],dnl
      [AC_MSG_NOTICE([Using $$1 as $2 executable.])])])

find_or_override_prog([SED], [sed])
find_or_override_prog([BTRFS], [btrfs])
find_or_override_prog([HERD], [herd])

guilemoduledir="${datarootdir}/guile/site/${GUILE_EFFECTIVE_VERSION}"
AC_SUBST([guilemoduledir])
AC_SUBST([GUILE_EFFECTIVE_VERSION])

AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([Makefile])

AC_OUTPUT
