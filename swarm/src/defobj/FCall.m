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
#include "fcall_objc.h"
#include "fcall_java.h"
#endif

#include <misc.h>
#include <objc/mframe.h>

#ifdef HAVE_JDK
#include <defobj/directory.h>
JNIEnv *jniEnv;
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

  /* bogus */
  java_static_call_functions[fcall_type_ulonglong] =
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticLongMethod);
  java_static_call_functions[fcall_type_slonglong] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticLongMethod);

  java_static_call_functions[fcall_type_float] =
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticFloatMethod);
  java_static_call_functions[fcall_type_double] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticDoubleMethod);
  java_static_call_functions[fcall_type_long_double] = NULL;
  java_static_call_functions[fcall_type_object] = NULL;
  java_static_call_functions[fcall_type_class] = NULL;
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
  java_call_functions[fcall_type_long_double] = NULL;
  java_call_functions[fcall_type_object] = NULL;
  java_call_functions[fcall_type_class] = NULL;
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
      objc_setup_call (fargs, self->fobject, self->fmethod);
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
      java_setup_call (fargs, jniEnv, self->fobject, self->fmethod);
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
      java_setup_static_call (fargs, jniEnv, self->fclass, self->fmethod);
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

#ifndef USE_AVCALL
static ffi_type *ffi_types[FCALL_TYPE_COUNT] = {
  &ffi_type_void,
  &ffi_type_uchar, &ffi_type_schar, 
  &ffi_type_ushort, &ffi_type_sshort, 
  &ffi_type_uint, &ffi_type_sint, 
  &ffi_type_ulong, &ffi_type_slong, 
  &ffi_type_uint64, &ffi_type_sint64, 
  // Note that some compilers may want to use double here
  &ffi_type_float, 
  &ffi_type_double,
  &ffi_type_longdouble,
  &ffi_type_pointer,
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
#ifdef HAVE_JDK
  if (fc->callType == javacall || fc->callType == javastaticcall)
    {
      java_set_return_type (fc, fa);
      fillHiddenArguments (fc);
      for (i = 0; i < fa->assignedArgumentCount; i++)
        {
          unsigned pos = i + MAX_HIDDEN;
          
          java_add_primitive (fa, fa->argTypes[pos], fa->argValues[pos]);
        }
    }
  else
#endif
    {
      objc_set_return_type (fc, fa);
      fillHiddenArguments (fc);
      for (i = 0; i < fa->assignedArgumentCount; i++)
        {
          unsigned pos = i + MAX_HIDDEN;
          
          objc_add_primitive (fa, fa->argTypes[pos], fa->argValues[pos]);
        }
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
        fargs->resultVal.sint = VAL(int, ret);
        break;
      case fcall_type_uint:
        fargs->resultVal.uint = VAL(unsigned, ret);
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
        fargs->resultVal.slong = VAL(long, ret);
        break;
      case fcall_type_ulong:
        fargs->resultVal.ulong = VAL(unsigned long, ret);
        break;
      case fcall_type_slonglong:
        fargs->resultVal.slonglong = VAL(long long, ret);
        break;
      case fcall_type_ulonglong:
        fargs->resultVal.ulonglong = VAL(unsigned long long, ret);
        break;
      case fcall_type_float:
        fargs->resultVal._float = ret._float;
        break;
      case fcall_type_double:
        fargs->resultVal._double = ret._double;
        break;
      case fcall_type_long_double:
        fargs->resultVal._long_double = ret._long_double;
        break;
      case fcall_type_object:
        fargs->resultVal.object = VAL(id, ret);
        break;
      case fcall_type_class:
        fargs->resultVal.class = VAL(Class, ret);
        break;
      case fcall_type_selector:
        fargs->resultVal.selector = VAL(SEL, ret);
        break;
      case fcall_type_string:
        fargs->resultVal.string = VAL(const char *, ret);
        break;
#ifdef HAVE_JDK
      case fcall_type_jobject:
        fargs->resultVal.object = (id) VAL(jobject, ret);
        break;
      case fcall_type_jstring:
        fargs->resultVal.object = (id) VAL(jstring, ret);
        break;
#endif
      default:
        abort ();
      }
#else
#ifdef HAVE_JDK
  if (callType == javacall || callType == javastaticcall)
    java_call (fargs);
  else
#endif
    objc_call (fargs);
#endif
}

- (void *)getResult
{
  return [fargs getResult];
}

- (retval_t)getRetVal: (retval_t)retVal buf: (types_t *)buf
{
  types_t *res = &((FArguments_c *) fargs)->resultVal;

#ifdef HAVE_JDK
  id return_jobject (void)
    {
      return SD_FINDOBJC (jniEnv, (jobject) buf->object);
    }
  const char *return_jstring (void)
    {
      const char *newString =
        swarm_directory_copy_java_string (jniEnv, (jstring) buf->object);
      
      (void) SD_SWITCHOBJC (jniEnv, (jstring) buf->object, (id) newString);
      return newString;
    }
#endif

  *buf = *res;

#ifndef BUGGY_BUILTIN_APPLY
  {
    char return_schar (void) { return buf->schar; }
    unsigned char return_uchar (void) { return buf->uchar; }
    short return_sshort (void) { return buf->sshort; }
    unsigned short return_ushort (void) { return buf->ushort; }
    int return_sint (void) { return buf->sint; }
    unsigned return_uint (void) { return buf->uint; }
    long return_slong (void) { return buf->slong; }
    unsigned long return_ulong (void) { return buf->ulong; }
    long long return_slonglong (void) { return buf->slonglong; }
    unsigned long long return_ulonglong (void) { return buf->ulonglong; }
    const char *return_string (void) { return buf->string; }
    float return_float (void) { return buf->_float; }
    double return_double (void) { return buf->_double; }
    long double return_long_double (void) { return buf->_long_double; }
    id return_object (void) { return buf->object; }
    Class return_class (void) { return buf->class; }
    void return_void (void) { return; }
    void apply (apply_t func)
      {
        memcpy (retVal,
                __builtin_apply (func, __builtin_apply_args (), 16),
                MFRAME_RESULT_SIZE);
      }
#define APPLY(func) apply ((apply_t) func)
    
    switch (fargs->returnType)
      {
      case fcall_type_void:
        APPLY (return_void);
        break;
      case fcall_type_uchar: 
        APPLY (return_uchar);
        break;
      case fcall_type_schar:
        APPLY (return_schar);
        break;
      case fcall_type_ushort:
        APPLY (return_ushort);
        break;
      case fcall_type_sshort: 
        APPLY (return_sshort);
        break;
      case fcall_type_uint:
        APPLY (return_uint);
        break;
      case fcall_type_sint:
        APPLY (return_sint);
        break;
      case fcall_type_ulong: 
        APPLY (return_ulong);
        break;
      case fcall_type_slong:
        APPLY (return_ulong);
        break;
      case fcall_type_ulonglong: 
        APPLY (return_ulonglong);
        break;
      case fcall_type_slonglong:
        APPLY (return_ulonglong);
        break;
      case fcall_type_float: 
        APPLY (return_float);
        break;
      case fcall_type_double:
        APPLY (return_double);
        break;
      case fcall_type_long_double:
        APPLY (return_long_double);
        break;
      case fcall_type_string:
        APPLY (return_string);
        break;
      case fcall_type_object:
        APPLY (return_object);
        break;
      case fcall_type_class:
        APPLY (return_class);
        break;
#ifdef HAVE_JDK
      case fcall_type_jobject:
        APPLY (return_jobject);
        break;
      case fcall_type_jstring:
        APPLY (return_jstring);
        break;
#endif
      default:
        abort ();
      }
    return retVal;
  }
#else
  {
    void *ptr;
    
    switch (fargs->returnType)
      {
      case fcall_type_void:
        ptr = NULL;
        break;
      case fcall_type_uchar:
#ifdef __sparc__
        buf->uint = buf->uint >> 24;
        ptr = &buf->uint;
#else
        ptr = &buf->uchar;
#endif
        break;
      case fcall_type_schar:
#ifdef __sparc__
        buf->sint = buf->sint >> 24;
        ptr = &buf->sint;
#else
        ptr = &buf->schar;
#endif
        break;
      case fcall_type_ushort:
#ifdef __sparc__
        buf->uint = buf->uint >> 16;
        ptr = &buf->uint;
#else
        ptr = &buf->ushort;
#endif
        break;
      case fcall_type_sshort:
#ifdef __sparc__
        buf->sint = buf->sint >> 16;
        ptr = &buf->sint;
#else
        ptr = &buf->sshort;
#endif
        break;
      case fcall_type_sint:
        ptr = &buf->sint;
        break;
      case fcall_type_uint:
        ptr = &buf->uint;
        break;
      case fcall_type_ulong:
        ptr = &buf->ulong;
        break;
      case fcall_type_slong:
        ptr = &buf->slong;
        break;
      case fcall_type_ulonglong:
        ptr = &buf->ulonglong;
        break;
      case fcall_type_slonglong:
        ptr = &buf->slonglong;
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
      case fcall_type_long_double:
        ptr = &buf->_long_double;
#ifdef __sparc__
        ptr -= 8; /* untested */
#endif
        break;
      case fcall_type_object:
        ptr = &buf->object;
        break;
      case fcall_type_class:
        ptr = &buf->class;
        break;
      case fcall_type_string:
        ptr = &buf->string;
        break;
#ifdef HAVE_JDK
      case fcall_type_jobject:
        buf->object = return_jobject ();
        ptr = &buf->object;
        break;
      case fcall_type_jstring:
        buf->string = return_jstring ();
        ptr = &buf->string;
        break;
#endif
      default:
        abort ();
      }
    return ptr;
  }
#endif
}

@end
