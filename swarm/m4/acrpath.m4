AC_DEFUN(md_CHECK_RPATH,
[AC_MSG_CHECKING(rpath option)
case "`(uname -sr) 2>/dev/null`" in
 "SunOS 5"*)
   ac_xsave_LIBS="$LIBS"; LIBS="$LIBS -R$libdir"
   AC_TRY_LINK(, , RPATH='-R', RPATH='-R ')
   LIBS="$ac_xsave_LIBS"
   ;;
 HP-UX*)
   RPATH='-L'
   ;;
 *)
   RPATH='-Wl,-rpath,'
   ;;
esac
AC_MSG_RESULT(\"$RPATH\")
])
    
