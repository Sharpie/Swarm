AC_DEFUN([md_CHECK_ARGP],
[AC_CHECK_HEADERS(argp.h)
if test $ac_cv_header_argp_h = no; then
  ARGP_H=argp.h
else
  ARGP_H=
fi
AC_SUBST(ARGP_H)
AC_CHECK_FUNC(argp_parse,,
	[AC_LIBOBJ(argp-ba)
	AC_LIBOBJ(argp-eexst)
	AC_LIBOBJ(argp-fmtstream)
	AC_LIBOBJ(argp-fs-xinl)
	AC_LIBOBJ(argp-help)
	AC_LIBOBJ(argp-parse)
	AC_LIBOBJ(argp-pv)
	AC_LIBOBJ(argp-pvh)
	AC_LIBOBJ(argp-xinl)
	AC_LIBOBJ(getopt1)
	AC_LIBOBJ(getopt)
	])
])

