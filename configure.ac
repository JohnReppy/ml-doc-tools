dnl Process this file with autoconf to produce a configure script.
dnl
dnl COPYRIGHT (c) 2022 John Reppy (http:www.cs.uchicago.edu/~jhr)
dnl All rights reserved.
dnl
dnl This the configuration script for autoconf version 2.52+.  The
dnl file configure.in is for older versions.

AC_INIT([ML-Doc],[2.20])

AC_PREREQ([2.71])
AC_COPYRIGHT([COPYRIGHT (c) 2022 John Reppy] (http://www.cs.uchicago.edu/~jhr))
AC_CONFIG_SRCDIR(tools/lib/mldoc/mldoc-dtd.sml)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)

AC_PREFIX_DEFAULT(.)

dnl
dnl include additional macros
dnl
sinclude(config/check_smlnj.m4)

AC_MSG_CHECKING([prefix directory])
if test "x$prefix" != xNONE; then
  prefix_dir=`(cd $prefix; pwd)`
else
  prefix_dir=`(cd $ac_default_prefix; pwd)`
fi
AC_MSG_RESULT([$prefix_dir])

MLDOC_BIN=$prefix_dir/bin
AC_SUBST(MLDOC_BIN)

MLDOC_LIB=`pwd`/lib
AC_SUBST(MLDOC_LIB)

INSTALL_SMLNJ_WRAPPER=$MLDOC_BIN/install-sml-wrapper.sh
AC_SUBST(INSTALL_SMLNJ_WRAPPER)

dnl
dnl standard tools
dnl
AC_PROG_INSTALL

#################### SGMLS (or ilk) configuration ####################

dnl
dnl locate nsgmls
dnl
AC_PATH_PROGS(NSGMLS, nsgmls onsgmls, no)
if test x$NSGMLS = xno ; then
  AC_MSG_ERROR([must have nsgmls/onsgmls installed])
fi
AC_SUBST(NSGMLS)

#################### SML configuration ####################

dnl
dnl We support building with both SML/NJ (the default) or MLton.
dnl

dnl first check for --with-mlton=path flag
dnl
AC_ARG_WITH([mlton],
  [AS_HELP_STRING([--with-mlton=<path-to-mlton>],
    [use the MLton Standard ML compiler to build the ML-Doc tools])],
  [with_mlton=yes],[with_mlton=no])
if test x"$with_mlton" = xyes ; then
  if test x"$ac_cv_path_with_mlton" = x ; then
    # look for mlton in the PATH
    AC_PATH_PROGS(with_mlton, mlton, no)
    if test x"$MLTON" = xno ; then
      AC_MSG_ERROR([unable to find mlton executable; please specify the path])
    fi
  else
    with_mlton=$ac_cv_path_with_mlton
    case $with_mlton in
      /*) ;;
      *) AC_MSG_ERROR([please specify absolute path for mlton executable]) ;;
    esac
    if test ! -x $with_mlton ; then
      AC_MSG_ERROR([invalid path for mlton; $with_mlton is not executable])
    fi
  fi
fi

if test x"$with_mlton" = xno ; then
  # we are using SML/NJ
  #

  SMLNJ_SIZE=""
  if test x"$SIZE_32" = xyes -a x"$SIZE_64" = xyes ; then
    AC_MSG_ERROR([cannot specify both 32 and 64-bit targets])
  elif test x"$SIZE_32" = xyes ; then
    SMLNJ_SIZE="-32"
  elif test x"$SIZE_64" = xyes ; then
    SMLNJ_SIZE="-64"
  fi
  AC_SUBST(SMLNJ_SIZE)

  dnl check that we have a compatible version of SML/NJ
  dnl
  CHECK_SMLNJ([AC_MSG_ERROR([unable to find SML/NJ installation; please add it to your PATH or set SML_CMD])])
  if test $SMLNJ_MAJOR_VERSION -lt 110 \
    -o $SMLNJ_MINOR_VERSION -lt 99
  then
    AC_MSG_ERROR([installation requires SML/NJ version 110.99+ (available from smlnj.org)])
  fi

  dnl look for ml-makedepend
  dnl
  tmpPATH="$SMLNJ_PATH:$PATH"
  AC_PATH_PROG(ML_MAKEDEPEND, ml-makedepend, ":", $tmpPATH)
  AC_SUBST(ML_MAKEDEPEND)

  dnl
  dnl look for ml-build
  dnl
  tmpPATH="$SMLNJ_PATH:$PATH"
  AC_PATH_PROG(ML_BUILD, ml-build, none, $tmpPATH)
  if test $ML_BUILD = none ; then
    AC_MSG_ERROR([ml-build not found])
  fi
  AC_SUBST(ML_BUILD)
  SML=smlnj

  HEAP_IMAGE_DIR=$MLDOC_BIN/.heap
  INSTALL_SMLNJ_WRAPPER=$MLDOC_BIN/install-sml-wrapper.sh
  AC_SUBST(HEAP_IMAGE_DIR)
  AC_SUBST(INSTALL_SMLNJ_WRAPPER)

else

  # we are using MLton
  MLTON=$with_mlton
  AC_SUBST(MLTON)
  SML=mlton

fi
AC_SUBST(SML)

#################### end SML configuration ####################

#################### write output ####################

dnl
dnl write output
dnl
AC_CONFIG_FILES(\
  Makefile \
  tools/extract-info/Makefile \
  tools/extract-sig/Makefile \
  tools/filter-index/Makefile \
  tools/html-gen/Makefile \
  tools/html-index/Makefile \
  tools/html-toc/Makefile \
  tools/latex-gen/Makefile \
  tools/merge-info/Makefile \
  tools/mkdoc/Makefile \
  tools/mk-mldoc-makefile/Makefile \
  tools/proof-latex/Makefile \
  config/run-latex \
  tools/lib/sgmls-input/run-sgmls.sml:tools/lib/sgmls-input/run-sgmls_sml.in \
  tools/lib/config/std-options.sml:tools/lib/config/std-options_sml.in \
  tools/mk-mldoc-makefile/bin-dir.sml:tools/mk-mldoc-makefile/bin-dir_sml.in \
)

AC_CONFIG_COMMANDS_PRE([if test ! -d bin ; then mkdir bin; fi])
AC_CONFIG_COMMANDS_PRE([if test ! -d bin/.heap ; then mkdir bin/.heap; fi])

AC_CONFIG_COMMANDS([default-1],[[chmod +x bin/install-sml-wrapper.sh $ML_BUILD_SCRIPT]],[[]])
dnl ***** shell scripts *****
AC_CONFIG_FILES([bin/install-sml-wrapper.sh:config/install-sml-wrapper_sh.in], [chmod +x bin/install-sml-wrapper.sh])

AC_OUTPUT

#################### end write output ####################
