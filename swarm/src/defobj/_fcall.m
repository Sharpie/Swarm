// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#ifdef OBJC
#define AVALIST(fa) fa->objc_avalist
#define ADD_PRIMITIVE objc_add_primitive
#define SET_RETURN_TYPE objc_set_return_type
#define AV_CALL objc_call
#endif

#ifdef JAVA
#define AVALIST(fa) fa->java_avalist
#define ADD_PRIMITIVE java_add_primitive
#define SET_RETURN_TYPE java_set_return_type
#define AV_CALL java_call
#endif

#import <defobj/FArguments.h>
#include <swarmconfig.h> // HAVE_JDK
#ifdef HAVE_JDK
#import "java.h"
#endif

void
ADD_PRIMITIVE (FArguments_c *fa, fcall_type_t type, void *val)
{
  switch (type)
    {
    case fcall_type_void:
      abort ();
    case fcall_type_boolean:
    case fcall_type_uchar:
      av_uchar (AVALIST (fa), *(unsigned char *) val);
      break;
    case fcall_type_schar:
      av_char (AVALIST (fa), *(char *) val);
      break;
    case fcall_type_ushort:
      av_ushort (AVALIST (fa), *(unsigned short *) val);
      break;
    case fcall_type_sshort:
      av_short (AVALIST (fa), *(short *) val);
      break;
    case fcall_type_uint:
      av_uint (AVALIST (fa), *(unsigned *) val);
      break;
    case fcall_type_sint:
      av_int (AVALIST (fa), *(int *) val);
      break;
    case fcall_type_slong:
      av_long (AVALIST (fa), *(long *) val);
      break;
    case fcall_type_ulong:
      av_ulong (AVALIST (fa), *(unsigned long *) val);
      break;
    case fcall_type_slonglong:
      av_longlong (AVALIST (fa), *(long long *) val);
      break;
    case fcall_type_ulonglong:
      av_ulonglong (AVALIST (fa), *(unsigned long long *) val);
      break;
    case fcall_type_float:
      av_float (AVALIST (fa), *(float *) val);
      break;
    case fcall_type_double:
      av_double (AVALIST (fa), *(double *) val);
      break;
    case fcall_type_long_double:
      abort ();
      break;
    case fcall_type_string:
      av_ptr (AVALIST (fa), const char *, *(const char **) val);
      break;
    case fcall_type_selector:
      av_ptr (AVALIST (fa), SEL, *(SEL *) val);
      break;
    case fcall_type_object:
      av_ptr (AVALIST (fa), id, *(id *) val);
      break;
    case fcall_type_class:
      av_ptr (AVALIST (fa), Class, *(Class *) val);
      break;
#ifdef HAVE_JDK
    case fcall_type_jobject:
    case fcall_type_jselector:
      av_ptr (AVALIST (fa), jobject, *(jobject *) val);
      break;
    case fcall_type_jstring:
      av_ptr (AVALIST (fa), jstring, *(jstring *) val);
      break;
#else
    case fcall_type_jobject:
    case fcall_type_jselector:
    case fcall_type_jstring:
      abort ();
#endif
    case fcall_type_iid:
      av_ptr (AVALIST (fa), void *, * (void **) val);
      break;
    }
}

void
SET_RETURN_TYPE (FCall_c *fc)
{
  FArguments_c *fa = fc->fargs;
  void (*func) (void) = fc->ffunction;

  switch (fa->retVal.type)
    {
    case fcall_type_void:
      av_start_void (AVALIST (fa), func);
      break;
    case fcall_type_boolean:
    case fcall_type_uchar:
      av_start_uchar (AVALIST (fa), func, &fa->retVal.val.uchar);
      break;
    case fcall_type_schar:
      av_start_char (AVALIST (fa), func, &fa->retVal.val.schar);
      break;
    case fcall_type_ushort:
      av_start_ushort (AVALIST (fa), func, &fa->retVal.val.ushort);
      break;
    case fcall_type_sshort:
      av_start_short (AVALIST (fa), func, &fa->retVal.val.sshort);
      break;
    case fcall_type_uint:
      av_start_uint (AVALIST (fa), func, &fa->retVal.val.uint);
      break;
    case fcall_type_sint:
      av_start_int (AVALIST (fa), func, &fa->retVal.val.sint);
      break;
    case fcall_type_ulong:
      av_start_ulong (AVALIST (fa), func, &fa->retVal.val.ulong);
      break;
    case fcall_type_slong:
      av_start_long (AVALIST (fa), func, &fa->retVal.val.slong);
      break;
    case fcall_type_slonglong:
      av_start_long (AVALIST (fa), func, &fa->retVal.val.slonglong);
      break;
    case fcall_type_ulonglong:
      av_start_ulong (AVALIST (fa), func, &fa->retVal.val.ulonglong);
      break;
    case fcall_type_float:
      av_start_float (AVALIST (fa), func, &fa->retVal.val._float);
      break;
    case fcall_type_double:
      av_start_double (AVALIST (fa), func, &fa->retVal.val._double);
      break;
    case fcall_type_long_double:
      abort ();
      break;
    case fcall_type_object:
      av_start_ptr (AVALIST (fa), func, id, &fa->retVal.val.object);
      break;
    case fcall_type_string:
      av_start_ptr (AVALIST (fa), func, const char *, &fa->retVal.val.string);
      break;
    case fcall_type_selector:
      av_start_ptr (AVALIST (fa), func, SEL, &fa->retVal.val.selector);
      break;
    case fcall_type_class:
      av_start_ptr (AVALIST (fa), func, Class, &fa->retVal.val._class);
      break;
    case fcall_type_jobject:
    case fcall_type_jselector:
      av_start_ptr (AVALIST (fa), func, jobject, &fa->retVal.val.object);
      break;
    case fcall_type_jstring:
      av_start_ptr (AVALIST (fa), func, jstring, &fa->retVal.val.object);
      break;
    case fcall_type_iid:
      av_start_ptr (AVALIST (fa), func, void *, &fa->retVal.val.object);
      break;
    }
}


void
AV_CALL (FArguments_c *fa)
{
  av_call (AVALIST (fa));
}
