AC_DEFUN(md_CHECK_ARGP,
[AC_CHECK_FUNCS(argp_parse,,LIBOBJS="$LIBOBJS argp-ba.c argp-eexst.c argp-fmtstream.c argp-fs-xinl.c argp-help.c argp-parse.c argp-pv.c argp-pvh.c argp-xinl.c getopt1.c getopt.c")
])

