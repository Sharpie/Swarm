// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         FCall.m
Description:  foreign function call
Library:      defobj
*/

#import "FCall.h"
#import <defobj.h>
#import <objc/objc-api.h>

#include <swarmconfig.h>
#ifndef USE_AVCALL
#undef PACKAGE
#undef VERSION
#include <ffi.h>
#undef PACKAGE
#undef VERSION
#endif

#include <misc.h>

#ifdef HAVE_JDK
#include <jni.h>
JNIEnv *jniEnv;
#endif

extern void switch_to_ffi_types (FArguments * self);
static void fillHiddenArguments (FCall_c * self);

#ifdef HAVE_JDK
void * java_static_call_functions[number_of_types];
void * java_call_functions[number_of_types];
#endif

#ifndef HAVE_JDK
static void
java_not_available (void)
{
  raiseEvent (NotImplemented,
              "Java support not available on this configuration");
}
#endif

#ifdef HAVE_JDK
void 
defobj_init_java_call_tables (void *jEnv)
{
  java_static_call_functions[fcall_type_void] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticVoidMethod);
  java_static_call_functions[fcall_type_uchar] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticCharMethod);
  java_static_call_functions[fcall_type_schar] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticCharMethod);
  java_static_call_functions[fcall_type_ushort] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticShortMethod);
  java_static_call_functions[fcall_type_sshort] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticShortMethod);
  java_static_call_functions[fcall_type_uint] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticIntMethod);
  java_static_call_functions[fcall_type_sint] =
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticIntMethod);
  java_static_call_functions[fcall_type_ulong] =
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticLongMethod);
  java_static_call_functions[fcall_type_slong] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticLongMethod);
  java_static_call_functions[fcall_type_float] =
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticFloatMethod);
  java_static_call_functions[fcall_type_double] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticDoubleMethod);
  java_static_call_functions[fcall_type_object] = NULL;
  java_static_call_functions[fcall_type_string] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticObjectMethod);
  java_static_call_functions[fcall_type_jobject] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticObjectMethod);

  java_call_functions[fcall_type_void] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallVoidMethod);
  java_call_functions[fcall_type_uchar] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallCharMethod);
  java_call_functions[fcall_type_schar] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallCharMethod);
  java_call_functions[fcall_type_ushort] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallShortMethod);
  java_call_functions[fcall_type_sshort] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallShortMethod);
  java_call_functions[fcall_type_uint] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallIntMethod);
  java_call_functions[fcall_type_sint] =
      FFI_FN ((*(JNIEnv *)jEnv)->CallIntMethod);
  java_call_functions[fcall_type_ulong] =
      FFI_FN ((*(JNIEnv *)jEnv)->CallLongMethod);
  java_call_functions[fcall_type_slong] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallLongMethod);
  java_call_functions[fcall_type_float] =
      FFI_FN ((*(JNIEnv *)jEnv)->CallFloatMethod);
  java_call_functions[fcall_type_double] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallDoubleMethod);
  java_call_functions[fcall_type_object] = NULL;
  java_call_functions[fcall_type_string] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallObjectMethod);
  java_call_functions[fcall_type_jobject] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallObjectMethod);
}
#endif

static void 
fillHiddenArguments (FCall_c * self)
{
#ifndef USE_AVCALL
  switch (self->callType)
    {
    case objccall: 
      ((FArguments *)self->fargs)->hiddenArguments = 2;	
      ((FArguments *)self->fargs)->ffiArgTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      ((FArguments *)self->fargs)->argValues[MAX_HIDDEN - 2] = &self->fobject;
      ((FArguments *)self->fargs)->ffiArgTypes[MAX_HIDDEN - 1] = &ffi_type_pointer;
      ((FArguments *)self->fargs)->argValues[MAX_HIDDEN - 1] = &self->fmethod;
      break;
#ifdef HAVE_JDK
    case javacall:
      ((FArguments *)self->fargs)->hiddenArguments = 3;
      ((FArguments *)self->fargs)->ffiArgTypes[MAX_HIDDEN - 3] = &ffi_type_pointer;
      ((FArguments *)self->fargs)->argValues[MAX_HIDDEN - 3] = &jniEnv;
      ((FArguments *)self->fargs)->ffiArgTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      ((FArguments *)self->fargs)->argValues[MAX_HIDDEN - 2] = &self->fobject;
      ((FArguments *)self->fargs)->ffiArgTypes[MAX_HIDDEN - 1] = &ffi_type_pointer;
      ((FArguments *)self->fargs)->argValues[MAX_HIDDEN - 1] = &self->fmethod;
      break;
    case javastaticcall:
      ((FArguments *)self->fargs)->hiddenArguments = 3;
      ((FArguments *)self->fargs)->ffiArgTypes[MAX_HIDDEN - 3] = &ffi_type_pointer;
      ((FArguments *)self->fargs)->argValues[MAX_HIDDEN - 3] = &jniEnv;
      ((FArguments *)self->fargs)->ffiArgTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      ((FArguments *)self->fargs)->argValues[MAX_HIDDEN - 2] = &self->fclass;
      ((FArguments *)self->fargs)->ffiArgTypes[MAX_HIDDEN - 1] = &ffi_type_pointer;
      ((FArguments *)self->fargs)->argValues[MAX_HIDDEN - 1] = &self->fmethod;
      break;
#endif
    }
#else
  abort ();
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
                     java_call_functions[(unsigned) fargs->returnType] :
                     java_static_call_functions[(unsigned) fargs->returnType]);
        
        (jmethodID) fmethod = (callType == javacall ?
                               (*jniEnv)->GetMethodID (jniEnv, fclass, 
                                                       methodName, 
                                                       fargs->javaSignature) :
                               (*jniEnv)->GetStaticMethodID (jniEnv, fclass, 
                                                             methodName, 
                                                             fargs->javaSignature)); 
        if (!fmethod)
          raiseEvent (SourceMessage, "Could not find Java method!\n");
      }
#endif
  fillHiddenArguments (self);
#ifndef USE_AVCALL
  switch_to_ffi_types ((FArguments *) fargs);
  {
    unsigned res;
    
    res = ffi_prep_cif (&cif, FFI_DEFAULT_ABI, 
                        fargs->hiddenArguments + fargs->assignedArguments, 
                        (ffi_type *) fargs->returnType, 
                        (ffi_type **) fargs->argTypes + MAX_HIDDEN - 
                        fargs->hiddenArguments);
    if (_obj_debug && res != FFI_OK)
      raiseEvent (SourceMessage,
                  "Failed while preparing foreign function call closure!\n"); 
  }
#else
  abort ();
#endif
  setNextPhase(self);
  return self;
}

PHASE(Setting)

- setArguments: arguments
{
  self->fargs = arguments;
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
#ifndef USE_AVCALL
  ffunction = FFI_FN (get_imp ((Class) fclass, (SEL) fmethod));
#else
  abort ();
#endif
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
  java_not_available();
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
  ffi_call(&cif, ffunction, fargs->result, fargs->argValues + 
	   MAX_HIDDEN - fargs->hiddenArguments);  
#else
  abort ();
#endif
}

- (void *)getResult
{
  return [fargs getResult];
}

- (retval_t)getReturnVal
{
  void *res = [self getResult];
  unsigned char return_uchar (void) { return *(unsigned char *) res; }
  unsigned short return_ushort (void) { return *(unsigned short *) res; }
  unsigned return_unsigned (void) { return *(unsigned *) res; }
  unsigned long return_ulong (void) { return *(unsigned long *) res; }
  const char *return_string (void) { return *(const char **) res; }
  float return_float (void) { return *(float *) res; }
  double return_double (void) { return *(double *) res; }
  id return_object (void) { return *(id *) res; }
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
    default:
      abort ();
    }
}

@end
