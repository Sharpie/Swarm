AC_DEFUN(md_PROG_GS,
[AC_PATH_PROG(GS, gs, missing)
if test "$GS" = missing ; then
  unset ac_cv_path_GS
  unset GS
  AC_PATH_PROG(GS, gswin32c, missing)
  if test "$GS" = missing ; then
    AC_MSG_ERROR(Could not find GhostScript)
  fi
fi
for device in png256 ppm ; do
  AC_MSG_CHECKING(that GhostScript can convert eps to $device)
  outfile=out$$.png
  errs=`$GS -q -dNOPAUSE -sDEVICE=$device -sOutputFile=$outfile < $srcdir/figs/swarm.eps 2>&1`
  if test -n "$errs" ; then
    echo "[$errs]"
    AC_MSG_ERROR(Could not convert eps to $device.  Please get a better-equipped version of GhostScript)
  fi
  rm -f $outfile
  AC_MSG_RESULT(yes)
done
AC_SUBST(GS_LIB)
])

 
  
