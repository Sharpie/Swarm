AC_DEFUN(md_CHECK_ARGP,
[AC_CHECK_HEADERS(argp.h)
if test $ac_cv_header_argp_h = no; then
  ARGP_H=argp.h
else
  ARGP_H=
fi
AC_SUBST(ARGP_H)
AC_CHECK_FUNCS(argp_parse,,LIBOBJS="$LIBOBJS argp-ba.lo argp-eexst.lo argp-fmtstream.lo argp-fs-xinl.lo argp-help.lo argp-parse.lo argp-pv.lo argp-pvh.lo argp-xinl.lo getopt1.lo getopt.lo")
])

