AC_DEFUN(al_SWARM_LOAD,
[# Prefer explicitly selected file to automatically selected ones.
AC_MSG_CHECKING(Swarm configuration)
if test -z "$CONFIG_SWARM"; then
  if test -f "$swarmdir/etc/swarm/config.swarm"; then
    CONFIG_SWARM="$swarmdir/etc/swarm/config.swarm"
    AC_MSG_RESULT($CONFIG_SWARM)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR(Please use --with-swarm to specify correct installed location of swarm.)
  fi
fi
for swarm_config_file in $CONFIG_SWARM; do
  if test -r "$swarm_config_file"; then
     AC_MSG_RESULT(loading site script $swarm_config_file)
     . "$swarm_config_file"
  fi
done
]) 

AC_DEFUN(al_INIT_SWARM,
[ # hide the automake substitution

AC_ARG_WITH(swarm,dnl
changequote(<,>)dnl
  --with-swarm=DIR       Swarm installed at DIR [$SWARMHOME],dnl
changequote([,])dnl
  swarmdir=$withval,dnl
  swarmdir="$SWARMHOME")

al_SWARM_LOAD

AM_PROG_LIBTOOL
LIBTOOL='$(SHELL) $(swarm_bindir)/libtool-swarm'
AC_SUBST(LIBTOOL)

AC_SUBST(OBJC)
AC_SUBST(OBJCFLAGS)

AC_SUBST(swarm_prefix)
AC_SUBST(swarm_execprefix)
AC_SUBST(swarm_bindir)
AC_SUBST(swarm_includedir)
AC_SUBST(swarm_libdir)
AC_SUBST(swarm_datadir)
AC_SUBST(SWARM_LDADD)
AC_SUBST(SWARM_LDFLAGS)
])
