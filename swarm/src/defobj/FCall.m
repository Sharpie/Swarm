// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         FCall.m
Description:  foreign function call
Library:      defobj
*/

#import <defobj/FCall.h>
#import <defobj.h>
#import <defobj/FArguments.h>
#import <objc/objc-api.h>

#include <swarmconfig.h>
#ifndef USE_AVCALL
#undef PACKAGE
#undef VERSION
#include <ffi.h>
#undef PACKAGE
#undef VERSION
#else
#include <avcall.h>
#endif

#include <misc.h>

#ifdef HAVE_JDK
#include <jni.h>
JNIEnv *jniEnv;
#include <directory.h>
#endif

#ifdef HAVE_JDK
void * java_static_call_functions[FCALL_TYPE_COUNT];
void * java_call_functions[FCALL_TYPE_COUNT];
#endif

#ifndef HAVE_JDK
static void
java_not_available (void)
{
  raiseEvent (NotImplemented,
              "Java support not available on this configuration");
}
#endif

#define FUNCPTR(f) ((void (*)())f) 

#ifdef HAVE_JDK
void 
defobj_init_java_call_tables (void *jEnv)
{
  java_static_call_functions[fcall_type_void] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticVoidMethod);
  java_static_call_functions[fcall_type_uchar] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticCharMethod);
  java_static_call_functions[fcall_type_schar] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticCharMethod);
  java_static_call_functions[fcall_type_ushort] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticShortMethod);
  java_static_call_functions[fcall_type_sshort] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticShortMethod);
  java_static_call_functions[fcall_type_uint] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticIntMethod);
  java_static_call_functions[fcall_type_sint] =
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticIntMethod);
  java_static_call_functions[fcall_type_ulong] =
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticLongMethod);
  java_static_call_functions[fcall_type_slong] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticLongMethod);
  java_static_call_functions[fcall_type_float] =
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticFloatMethod);
  java_static_call_functions[fcall_type_double] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticDoubleMethod);
  java_static_call_functions[fcall_type_object] = NULL;
  java_static_call_functions[fcall_type_string] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticObjectMethod);
  java_static_call_functions[fcall_type_selector] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticObjectMethod);
  java_static_call_functions[fcall_type_jobject] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticObjectMethod);
  java_static_call_functions[fcall_type_jstring] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticObjectMethod);

  java_call_functions[fcall_type_void] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallVoidMethod);
  java_call_functions[fcall_type_uchar] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallCharMethod);
  java_call_functions[fcall_type_schar] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallCharMethod);
  java_call_functions[fcall_type_ushort] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallShortMethod);
  java_call_functions[fcall_type_sshort] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallShortMethod);
  java_call_functions[fcall_type_uint] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallIntMethod);
  java_call_functions[fcall_type_sint] =
      FUNCPTR ((*(JNIEnv *) jEnv)->CallIntMethod);
  java_call_functions[fcall_type_ulong] =
      FUNCPTR ((*(JNIEnv *) jEnv)->CallLongMethod);
  java_call_functions[fcall_type_slong] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallLongMethod);
  java_call_functions[fcall_type_float] =
      FUNCPTR ((*(JNIEnv *) jEnv)->CallFloatMethod);
  java_call_functions[fcall_type_double] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallDoubleMethod);
  java_call_functions[fcall_type_object] = NULL;
  java_call_functions[fcall_type_string] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallObjectMethod);
  java_call_functions[fcall_type_selector] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallObjectMethod);
  java_call_functions[fcall_type_jobject] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallObjectMethod);
  java_call_functions[fcall_type_jstring] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallObjectMethod);
}
#endif

static void 
fillHiddenArguments (FCall_c *self)
{
  FArguments_c *fargs = self->fargs;

  switch (self->callType)
    {
    case objccall: 
      fargs->hiddenArgumentCount = 2;	
#ifdef USE_AVCALL
      av_ptr (fargs->avalist, id, self->fobject);
      av_ptr (fargs->avalist, SEL, self->fmethod);
#else
      fargs->ffiArgTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 2] = &self->fobject;
      fargs->ffiArgTypes[MAX_HIDDEN - 1] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 1] = &self->fmethod;
#endif
      break;
#ifdef HAVE_JDK
    case javacall:
      fargs->hiddenArgumentCount = 3;
#ifdef USE_AVCALL
      av_ptr (fargs->avalist, JNIEnv *, jniEnv);
      av_ptr (fargs->avalist, jobject, self->fobject);
      av_ptr (fargs->avalist, jmethodID, self->fmethod);
#else
      fargs->ffiArgTypes[MAX_HIDDEN - 3] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 3] = &jniEnv;
      fargs->ffiArgTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 2] = &self->fobject;
      fargs->ffiArgTypes[MAX_HIDDEN - 1] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 1] = &self->fmethod;
#endif
      break;
    case javastaticcall:
      fargs->hiddenArgumentCount = 3;
#ifdef USE_AVCALL
      av_ptr (fargs->avalist, JNIEnv *, jniEnv);
      av_ptr (fargs->avalist, jclass, self->fclass);
      av_ptr (fargs->avalist, jmethodID, self->fmethod);
#else
      fargs->ffiArgTypes[MAX_HIDDEN - 3] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 3] = &jniEnv;
      fargs->ffiArgTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 2] = &self->fclass;
      fargs->ffiArgTypes[MAX_HIDDEN - 1] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 1] = &self->fmethod;
#endif
      break;
#endif
    }
}

#ifdef USE_AVCALL
static void
avcall_add_primitive (FArguments_c *fa, fcall_type_t type, void *val)
{
  switch (type)
    {
    case fcall_type_void:
      abort ();
    case fcall_type_uchar:
      av_uchar (fa->avalist, *(unsigned char *) val);
      break;
    case fcall_type_schar:
      av_char (fa->avalist, *(char *) val);
      break;
    case fcall_type_ushort:
      av_ushort (fa->avalist, *(unsigned short *) val);
      break;
    case fcall_type_sshort:
      av_short (fa->avalist, *(short *) val);
      break;
    case fcall_type_uint:
      av_uint (fa->avalist, *(unsigned *) val);
      break;
    case fcall_type_sint:
      av_int (fa->avalist, *(int *) val);
      break;
    case fcall_type_slong:
      av_long (fa->avalist, *(long *) val);
      break;
    case fcall_type_ulong:
      av_ulong (fa->avalist, *(unsigned long *) val);
      break;
    case fcall_type_float:
      av_float (fa->avalist, *(float *) val);
      break;
    case fcall_type_double:
      av_double (fa->avalist, *(double *) val);
      break;
    case fcall_type_string:
      av_ptr (fa->avalist, const char *, *(const char **) val);
      break;
    case fcall_type_selector:
      av_ptr (fa->avalist, SEL, *(SEL *) val);
      break;
#ifdef HAVE_JDK
    case fcall_type_jobject:
      av_ptr (fa->avalist, jobject, *(jobject *) val);
      break;
    case fcall_type_jstring:
      av_ptr (fa->avalist, jstring, *(jstring *) val);
      break;
#endif
    default:
      abort ();
    }
}
#endif

#ifndef USE_AVCALL
static ffi_type *ffi_types[FCALL_TYPE_COUNT] = {
  &ffi_type_void,
  &ffi_type_uchar, &ffi_type_schar, 
  &ffi_type_ushort, &ffi_type_sshort, 
  &ffi_type_uint, &ffi_type_sint, 
  &ffi_type_ulong, &ffi_type_slong, 
  // Note that some compilers may want to use double here
  &ffi_type_float, 
  &ffi_type_double, 
  &ffi_type_pointer,
  &ffi_type_pointer, 
  &ffi_type_pointer, 
  &ffi_type_pointer,
  &ffi_type_pointer};
#endif

static void
add_ffi_types (FCall_c *fc)
{
  unsigned i;
  FArguments_c *fa = fc->fargs;

#ifndef USE_AVCALL
  fa->ffiReturnType = ffi_types[fa->returnType];
  fillHiddenArguments (fc);
  for (i = 0; i < fa->assignedArgumentCount; i++)
    {
      unsigned pos = i + MAX_HIDDEN;
      fcall_type_t type = fa->argTypes[pos];
      
      fa->ffiArgTypes[pos] = ffi_types[type];
    }
#else
  void (*func) (void) = fc->ffunction;

  switch (fa->returnType)
    {
    case fcall_type_void:
      av_start_void (fa->avalist, func);
      break;
    case fcall_type_uchar:
      av_start_uchar (fa->avalist, func, &fa->resultVal.uchar);
      break;
    case fcall_type_schar:
      av_start_char (fa->avalist, func, &fa->resultVal.schar);
      break;
    case fcall_type_ushort:
      av_start_ushort (fa->avalist, func, &fa->resultVal.ushort);
      break;
    case fcall_type_sshort:
      av_start_short (fa->avalist, func, &fa->resultVal.sshort);
      break;
    case fcall_type_uint:
      av_start_uint (fa->avalist, func, &fa->resultVal.uint);
      break;
    case fcall_type_sint:
      av_start_int (fa->avalist, func, &fa->resultVal.sint);
      break;
    case fcall_type_ulong:
      av_start_ulong (fa->avalist, func, &fa->resultVal.ulong);
      break;
    case fcall_type_slong:
      av_start_long (fa->avalist, func, &fa->resultVal.slong);
      break;
    case fcall_type_float:
      av_start_float (fa->avalist, func, &fa->resultVal._float);
      break;
    case fcall_type_double:
      av_start_double (fa->avalist, func, &fa->resultVal._double);
      break;
    case fcall_type_object:
      av_start_ptr (fa->avalist, func, id, &fa->resultVal.object);
      break;
    case fcall_type_string:
      av_start_ptr (fa->avalist, func, const char *, &fa->resultVal.string);
      break;
    case fcall_type_selector:
      av_start_ptr (fa->avalist, func, SEL, &fa->resultVal.selector);
      break;
    case fcall_type_jobject:
      av_start_ptr (fa->avalist, func, jobject, &fa->resultVal.object);
      break;
    case fcall_type_jstring:
      av_start_ptr (fa->avalist, func, jstring, &fa->resultVal.object);
      break;
    default:
      abort ();
    }
  fillHiddenArguments (fc);
  for (i = 0; i < fa->assignedArgumentCount; i++)
    {
      unsigned pos = i + MAX_HIDDEN;
      fcall_type_t type = fa->argTypes[pos];
    
      avcall_add_primitive (fa, type, fa->argValues[pos]);
    }
#endif
}


@implementation FCall_c

PHASE(Creating)
+ createBegin: aZone
{
  FCall_c *newCall = [aZone allocIVars: self];
  newCall->fargs = NULL;
  return newCall;
}


- createEnd
{
  if (_obj_debug && (callType == ccall || callType == objccall) && !ffunction)
    raiseEvent (SourceMessage, "Function to be called not set!\n");
  if (_obj_debug && !fargs)
    raiseEvent (SourceMessage, "Arguments and return type not specified!\n");
#ifdef HAVE_JDK
  if (callType == javacall || callType == javastaticcall)
      {
        ffunction = (callType == javacall ? 
                     java_call_functions[fargs->returnType] :
                     java_static_call_functions[fargs->returnType]);
        
        (jmethodID) fmethod =
          (callType == javacall ?
           (*jniEnv)->GetMethodID (jniEnv, fclass, 
                                   methodName, 
                                   fargs->javaSignature) :
           (*jniEnv)->GetStaticMethodID (jniEnv, fclass, 
                                         methodName, 
                                         fargs->javaSignature)); 
        if (!fmethod)
          raiseEvent (SourceMessage, "Could not find Java method: `%s' (%s)\n",
                      methodName, fargs->javaSignature);
      }
#endif
  add_ffi_types (self);
#ifndef USE_AVCALL
  {
    unsigned res;
    
    res = ffi_prep_cif (&cif, FFI_DEFAULT_ABI, 
                        (fargs->hiddenArgumentCount +
                         fargs->assignedArgumentCount),
                        (ffi_type *) fargs->ffiReturnType, 
                        (ffi_type **) fargs->ffiArgTypes + MAX_HIDDEN - 
                        fargs->hiddenArgumentCount);
    if (_obj_debug && res != FFI_OK)
      raiseEvent (SourceMessage,
                  "Failed while preparing foreign function call closure!\n"); 
  }
#endif
  setNextPhase (self);
  return self;
}

PHASE(Setting)

- setArguments: arguments
{
  fargs = arguments;
  return self;
}

- getArguments
{
  return fargs;
}

- setFunctionPointer: (func_t)fn
{
  callType = ccall;
  ffunction = fn;
  return self;
}

- setMethod: (SEL)mtd inObject: obj
{
  Class cl;

  callType = objccall;
  (id) fobject = obj;
  (SEL) fmethod = mtd;
  cl = getClass (obj);
  (Class) fclass = cl;
  ffunction = FUNCPTR (get_imp ((Class) fclass, (SEL) fmethod));
  return self;
}

- setJavaMethod: (const char *)mtdName inObject: (JOBJECT)obj
{
#ifdef HAVE_JDK
  callType = javacall;
  (jclass) fclass = (*jniEnv)->GetObjectClass (jniEnv, obj);
  methodName = (char *) mtdName;
  (jobject) fobject = obj;
#else
  java_not_available ();
#endif
  return self;
}    
   
- setJavaMethod: (const char *)mtdName inClass: (const char *)className
{ 
#ifdef HAVE_JDK
  callType = javastaticcall;
  (jclass) fclass = (*jniEnv)->FindClass (jniEnv, className);
  methodName = (char *) mtdName;
#else
  java_not_available();
#endif
  return self;
}


PHASE(Using)

- (void)performCall
{
#ifndef USE_AVCALL
  types_t ret;

  ffi_call (&cif, ffunction, &ret, fargs->argValues + 
            MAX_HIDDEN - fargs->hiddenArgumentCount);  

#ifdef __mips64
#define VAL(type, var) (*((type *)(((void *)&var)+(sizeof(var)-sizeof(type)))))
#else
#define VAL(type, var) (*((type *)&var))
#endif

  switch (fargs->returnType)
      {
      case fcall_type_void:
        break;
      case fcall_type_schar:
        // character return is broken in libffi-1.18
        fargs->resultVal.schar = VAL(int, ret);
        break;
      case fcall_type_uchar:
        // character return is broken in libffi-1.18
        fargs->resultVal.uchar = VAL(unsigned, ret);
        break;
      case fcall_type_sint:
        fargs->resultVal.sint = ret.sint;
        break;
      case fcall_type_uint:
        fargs->resultVal.uint = ret.uint;
        break;
      case fcall_type_sshort:
        // short return is broken in libffi-1.18
        fargs->resultVal.sshort = VAL(int, ret);
        break;
      case fcall_type_ushort:
        // short return is broken in libffi-1.18
        fargs->resultVal.ushort = VAL(unsigned, ret);
        break;
      case fcall_type_slong:
        fargs->resultVal.slong = ret.slong;
        break;
      case fcall_type_ulong:
        fargs->resultVal.ulong = ret.ulong;
        break;
      case fcall_type_float:
        fargs->resultVal._float = ret._float;
        break;
      case fcall_type_double:
        fargs->resultVal._double = ret._double;
        break;
      case fcall_type_object:
        fargs->resultVal.object = VAL(id, ret);
        break;
      case fcall_type_selector:
        fargs->resultVal.selector = VAL(SEL, ret);
        break;
      case fcall_type_string:
        fargs->resultVal.string = VAL(const char *, ret);
        break;
#ifdef HAVE_JDK
      case fcall_type_jobject:
        fargs->resultVal.object = VAL(jobject, ret);
        break;
      case fcall_type_jstring:
        fargs->resultVal.object = VAL(jstring, ret);
        break;
#endif
      default:
        abort ();
      }
#else
  av_call (fargs->avalist);
#endif
}

- (void *)getResult
{
  return [fargs getResult];
}

- (retval_t)getRetVal: (types_t *)buf
{
  types_t *res = &((FArguments_c *) fargs)->resultVal;

#ifndef BUGGY_BUILTIN_APPLY
  unsigned char return_uchar (void) { return res->uchar; }
  unsigned short return_ushort (void) { return res->ushort; }
  unsigned return_unsigned (void) { return res->uint; }
  unsigned long return_ulong (void) { return res->ulong; }
  const char *return_string (void) { return res->string; }
  float return_float (void) { return res->_float; }
  double return_double (void) { return res->_double; }
  id return_object (void) { return res->object; }
#ifdef HAVE_JDK
  id return_jobject (void)
    {
      return JFINDOBJC (jniEnv, (jobject) res->object);
    }
  const char *return_jstring (void)
    {
      return java_copy_string (jniEnv, (jstring) res->object);
    }
#endif
  void return_void (void) { return; }

  retval_t apply_uchar (void)
    {
      void *args = __builtin_apply_args ();
      return __builtin_apply ((apply_t) return_uchar, args, sizeof (void *));
    }
  retval_t apply_ushort (void)
    {
      void *args = __builtin_apply_args ();
      return __builtin_apply ((apply_t) return_ushort, args, sizeof (void *));
    }
  retval_t apply_unsigned (void)
    {
      void *args = __builtin_apply_args ();
      return __builtin_apply ((apply_t) return_unsigned, args, sizeof (void *));
    }
  retval_t apply_ulong (void)
    {
      void *args = __builtin_apply_args ();
      return __builtin_apply ((apply_t) return_ulong, args, sizeof (void *));
    }
  retval_t apply_float (void)
    {
      void *args = __builtin_apply_args ();
      return __builtin_apply ((apply_t) return_float, args, sizeof (void *));
    }
  retval_t apply_double (void)
    {
      void *args = __builtin_apply_args ();
      return __builtin_apply ((apply_t) return_double, args, sizeof (void *));
    }
  retval_t apply_object (void)
    {
      void *args = __builtin_apply_args ();
      return __builtin_apply ((apply_t) return_object, args, sizeof (void *));
    }
  retval_t apply_string (void)
    {
      void *args = __builtin_apply_args ();
      return __builtin_apply ((apply_t) return_string, args, sizeof (void *));
    }
  retval_t apply_void (void)
    {
      void* args = __builtin_apply_args ();
      return __builtin_apply ((apply_t) return_void, args, sizeof (void *));
    }
#ifdef HAVE_JDK
  retval_t apply_jobject (void)
    {
      void *args = __builtin_apply_args ();
      return __builtin_apply ((apply_t) return_jobject, args, sizeof (void *));
    }
  retval_t apply_jstring (void)
    {
      void *args = __builtin_apply_args ();
      return __builtin_apply ((apply_t) return_jstring, args, sizeof (void *));
    }
#endif
  switch (fargs->returnType)
    {
    case fcall_type_void:
      return apply_void ();
    case fcall_type_uchar: 
    case fcall_type_schar:
      return apply_uchar ();
    case fcall_type_ushort:
    case fcall_type_sshort: 
      return apply_ushort ();
    case fcall_type_uint:
    case fcall_type_sint:
      return apply_unsigned ();
    case fcall_type_ulong: 
    case fcall_type_slong:
      return apply_ulong ();
    case fcall_type_float: 
      return apply_float ();
    case fcall_type_double:
      return apply_double ();
    case fcall_type_string:
      return apply_string ();
    case fcall_type_object:
      return apply_object ();
#ifdef HAVE_JDK
    case fcall_type_jobject:
      return apply_jobject ();
    case fcall_type_jstring:
      return apply_jobject ();
#endif
    default:
      abort ();
    }
#else
  void *ptr;
  *buf = *res;

  switch (fargs->returnType)
    {
    case fcall_type_void:
      ptr = NULL;
      break;
    case fcall_type_uchar:
      ptr = &buf->uchar;
      break;
    case fcall_type_schar:
      ptr = &buf->schar;
      break;
    case fcall_type_ushort:
      ptr = &buf->ushort;
      break;
    case fcall_type_sint:
      ptr = &buf->sshort;
      break;
    case fcall_type_ulong:
      ptr = &buf->ulong;
      break;
    case fcall_type_slong:
      ptr = &buf->slong;
      break;
    case fcall_type_float:
      ptr = &buf->_float;
#ifdef __sparc__
      ptr -= 8;
#endif
      break;
    case fcall_type_double:
      ptr = &buf->_double;
#ifdef __sparc__
      ptr -= 8;
#endif
      break;
    case fcall_type_object:
      ptr = &buf->object;
      break;
    case fcall_type_string:
      ptr = &buf->string;
      break;
#ifdef HAVE_JDK
    case fcall_type_jobject:
      buf->object = JFINDOBJC (jniEnv, (jobject) buf->object);
      ptr = &buf->object;
      break;
    case fcall_type_jstring:
      {
        const char *newString =
          java_copy_string (jniEnv, (jstring) buf->object);
        
        JUPDATE (jniEnv, (jstring) buf->object, (id) newString);
        buf->string = newString;
      }
      ptr = &buf->string;
      break;
#endif
    default:
      abort ();
    }
  return ptr;
#endif
}

@end
