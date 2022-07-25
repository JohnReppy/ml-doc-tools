dnl Process this file with autoconf to produce a configure script.
dnl
dnl COPYRIGHT (c) 2012 John Reppy (http:www.cs.uchicago.edu/~jhr)
dnl All rights reserved.
dnl
dnl This the configuration script for autoconf version 2.52+.  The
dnl file configure.in is for older versions.

AC_INIT([ML-Doc],[2.10])

AC_PREREQ([2.71])
AC_COPYRIGHT([COPYRIGHT (c) 2012 John Reppy] (http://www.cs.uchicago.edu/~jhr))
AC_CONFIG_SRCDIR(tools/lib/mldoc/mldoc-dtd.sml)
AC_CONFIG_AUX_DIR(config)

AC_PREFIX_DEFAULT(.)

bindir=`pwd`/bin
heapdir=$bindir/.heap
AC_SUBST(heapdir)

AC_MSG_CHECKING([checking prefix directory])
if test "x$prefix" != xNONE; then
  prefix_dir=`(cd $prefix; pwd)`
else
  prefix_dir=`(cd $ac_default_prefix; pwd)`
fi
AC_MSG_RESULT([$prefix_dir])
mldoc_bindir=$prefix_dir/bin
AC_SUBST(mldoc_bindir)
mldoc_libdir=`pwd`/lib
AC_SUBST(mldoc_libdir)

INSTALL_SMLNJ_WRAPPER=$bindir/install-sml-wrapper.sh
AC_SUBST(INSTALL_SMLNJ_WRAPPER)

dnl
dnl include extra macros
dnl
sinclude(config/check_smlnj.m4)
sinclude(config/check_smlnj_heap_suffix.m4)

dnl
dnl standard tools
dnl
AC_PROG_INSTALL

dnl
dnl check that we have a compatible version of SML/NJ
dnl
CHECK_SMLNJ([AC_MSG_ERROR([must have SML/NJ installed])])
if test $SMLNJ_MAJOR_VERSION -eq 110 \
  -a $SMLNJ_MINOR_VERSION -ge 99
then
  USING_NEW_CM="TRUE"
else
  AC_MSG_ERROR([installation requires version 110.99+ of SML/NJ])
fi

dnl
dnl determine the heap-image suffix
dnl
CHECK_SMLNJ_HEAP_SUFFIX([
  AC_MSG_ERROR([unsupported configuration ${host_cpu}-${host_os}])])

dnl
dnl locate nsgmls
dnl
AC_PATH_PROGS(NSGMLS, nsgmls onsgmls, no)
if test x$NSGMLS = xno ; then
  AC_MSG_ERROR([must have nsgmls/onsgmls installed])
fi
AC_SUBST(NSGMLS)

dnl
dnl setup things so that we use the correct version of build-ml
dnl
if test $USING_NEW_CM = FALSE ; then
  ML_BUILD="$mldoc_bindir/ml-build"
else
  AC_PATH_PROG(ML_BUILD, ml-build, none, $SMLNJ_PATH:$PATH)
  if test $ML_BUILD = none ; then
    AC_MSG_ERROR([ml-build script not found])
  fi
fi
AC_SUBST(ML_BUILD)

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
if test $USING_NEW_CM = FALSE ; then
  AC_CONFIG_FILES([bin/ml-build:config/ml-build.in], [chmod +x bin/ml-build])
fi

AC_OUTPUT
