AC_DEFUN(al_SWARM_LOAD,
[# Prefer explicitly selected file to automatically selected ones.
AC_MSG_CHECKING(Swarm configuration)
if test -z "$CONFIG_SWARM"; then
  if test -f "$swarmdir/etc/config.swarm"; then
    CONFIG_SWARM="$swarmdir/etc/config.swarm"
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

AC_SUBST(swarmdir)
AC_SUBST(SWARM_LDADD)
AC_SUBST(SWARM_LDFLAGS)
])