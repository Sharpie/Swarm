AC_DEFUN(md_FIND_HDF5,
[test -z "$hdf5dir" && hdf5dir=$defaultdir
md_FIND_LIB(hdf5)
if test -n "$hdf5libdir" ; then
  md_FIND_INCLUDE(hdf5)
  HDF5INCLUDES=$_includes
  HDF5LDFLAGS=$_ldflags
  HDF5LIB=-lhdf5
  AC_DEFINE(HAVE_HDF5)
else
  HDF5INCLUDES=''
  HDF5LDFLAGS=''
  HDF5LIB=''
fi
AC_SUBST(hdf5libdir)
AC_SUBST(hdf5includedir)
AC_SUBST(HDF5LIB)
AC_SUBST(HDF5INCLUDES)
AC_SUBST(HDF5LDFLAGS)
])
