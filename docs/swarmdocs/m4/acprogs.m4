AC_DEFUN(md_PROG_GS,
[AC_CHECK_PROG(GS, gs, gs, missing)
if test "$GS" = missing ; then
  unset ac_cv_prog_GS
  unset GS
  AC_CHECK_PROG(GS, gswin32c, gswin32c, missing)
  if test "$GS" = missing ; then
    AC_MSG_ERROR(Could not find GhostScript)
  fi
fi
for device in png256 ppm ; do
  AC_MSG_CHECKING(that GhostScript can convert eps to $device)
  errs=`$GS -q -dNOPAUSE -sDEVICE=$device -sOutputFile=/dev/null $srcdir/figs/swarm.eps -c quit 2>&1`
  if test -n "$errs" ; then
    echo "[$errs]"
    AC_MSG_ERROR(Could not convert eps to $device.  Please get a better-equipped version of GhostScript)
  fi
  AC_MSG_RESULT(yes)
done
])

 
  
