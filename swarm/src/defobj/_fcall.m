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
#import <defobj/directory.h> // JNI things
#include <swarmconfig.h> // HAVE_JDK

void
ADD_PRIMITIVE (FArguments_c *fa, fcall_type_t type, void *val)
{
  switch (type)
    {
    case fcall_type_void:
      abort ();
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
#ifdef HAVE_JDK
    case fcall_type_jobject:
      av_ptr (AVALIST (fa), jobject, *(jobject *) val);
      break;
    case fcall_type_jstring:
      av_ptr (AVALIST (fa), jstring, *(jstring *) val);
      break;
#endif
    default:
      abort ();
    }
}

void
SET_RETURN_TYPE (FCall_c *fc, FArguments_c *fa)
{
  void (*func) (void) = fc->ffunction;

  switch (fa->returnType)
    {
    case fcall_type_void:
      av_start_void (AVALIST (fa), func);
      break;
    case fcall_type_uchar:
      av_start_uchar (AVALIST (fa), func, &fa->resultVal.uchar);
      break;
    case fcall_type_schar:
      av_start_char (AVALIST (fa), func, &fa->resultVal.schar);
      break;
    case fcall_type_ushort:
      av_start_ushort (AVALIST (fa), func, &fa->resultVal.ushort);
      break;
    case fcall_type_sshort:
      av_start_short (AVALIST (fa), func, &fa->resultVal.sshort);
      break;
    case fcall_type_uint:
      av_start_uint (AVALIST (fa), func, &fa->resultVal.uint);
      break;
    case fcall_type_sint:
      av_start_int (AVALIST (fa), func, &fa->resultVal.sint);
      break;
    case fcall_type_ulong:
      av_start_ulong (AVALIST (fa), func, &fa->resultVal.ulong);
      break;
    case fcall_type_slong:
      av_start_long (AVALIST (fa), func, &fa->resultVal.slong);
      break;
    case fcall_type_slonglong:
      av_start_long (AVALIST (fa), func, &fa->resultVal.slonglong);
      break;
    case fcall_type_ulonglong:
      av_start_ulong (AVALIST (fa), func, &fa->resultVal.ulonglong);
      break;
    case fcall_type_float:
      av_start_float (AVALIST (fa), func, &fa->resultVal._float);
      break;
    case fcall_type_double:
      av_start_double (AVALIST (fa), func, &fa->resultVal._double);
      break;
    case fcall_type_long_double:
      abort ();
      break;
    case fcall_type_object:
      av_start_ptr (AVALIST (fa), func, id, &fa->resultVal.object);
      break;
    case fcall_type_string:
      av_start_ptr (AVALIST (fa), func, const char *, &fa->resultVal.string);
      break;
    case fcall_type_selector:
      av_start_ptr (AVALIST (fa), func, SEL, &fa->resultVal.selector);
      break;
    case fcall_type_jobject:
      av_start_ptr (AVALIST (fa), func, jobject, &fa->resultVal.object);
      break;
    case fcall_type_jstring:
      av_start_ptr (AVALIST (fa), func, jstring, &fa->resultVal.object);
      break;
    default:
      abort ();
    }
}


void
AV_CALL (FArguments_c *fa)
{
  av_call (AVALIST (fa));
}
