AC_DEFUN(md_SWARM_INSTALLER,
[changequote(,)
install_prog=`echo $INSTALL | sed 's,.*/\([^/ ][^/ ]*\).*,\1,'`
install_prog_args=`echo $INSTALL | sed 's,.* \(.*\),\1,'`
install_data_args=`echo $INSTALL_DATA | sed 's,[^ ]* \(.*\),\1,'`
changequote([,])

SWARM_INSTALL=$install_prog
if test $install_prog = install-sh; then
  SWARM_INSTALL_PROGRAM='${swarm_bindir}'"/$install_prog $install_prog_args"
  SWARM_INSTALL_DATA_='${swarm_bindir}'"/$install_prog $install_prog_args $install_data_args"
else
  SWARM_INSTALL_PROGRAM=$INSTALL_PROGRAM
  SWARM_INSTALL_DATA_=$INSTALL_DATA
fi
AC_SUBST(SWARM_INSTALL)
AC_SUBST(SWARM_INSTALL_PROGRAM)
dnl avoid automake naming convention
AC_SUBST(SWARM_INSTALL_DATA_)
])
