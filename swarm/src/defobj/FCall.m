// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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
#include <misc.h>
#include <ffi.h>

#ifdef HAVE_JDK
#include <jni.h>
JNIEnv *jniEnv;
#endif

extern void switch_to_ffi_types (FArguments * self);

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
init_javacall_tables (void * jEnv)
{
  java_static_call_functions[swarm_type_void] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticVoidMethod);
  java_static_call_functions[swarm_type_uchar] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticCharMethod);
  java_static_call_functions[swarm_type_schar] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticCharMethod);
  java_static_call_functions[swarm_type_ushort] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticShortMethod);
  java_static_call_functions[swarm_type_sshort] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticShortMethod);
  java_static_call_functions[swarm_type_uint] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticIntMethod);
  java_static_call_functions[swarm_type_sint] =
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticIntMethod);
  java_static_call_functions[swarm_type_ulong] =
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticLongMethod);
  java_static_call_functions[swarm_type_slong] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticLongMethod);
  java_static_call_functions[swarm_type_float] =
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticFloatMethod);
  java_static_call_functions[swarm_type_double] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticDoubleMethod);
  java_static_call_functions[swarm_type_pointer] = NULL;
  java_static_call_functions[swarm_type_string] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticObjectMethod);
  java_static_call_functions[swarm_type_jobject] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallStaticObjectMethod);

  java_call_functions[swarm_type_void] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallVoidMethod);
  java_call_functions[swarm_type_uchar] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallCharMethod);
  java_call_functions[swarm_type_schar] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallCharMethod);
  java_call_functions[swarm_type_ushort] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallShortMethod);
  java_call_functions[swarm_type_sshort] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallShortMethod);
  java_call_functions[swarm_type_uint] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallIntMethod);
  java_call_functions[swarm_type_sint] =
      FFI_FN ((*(JNIEnv *)jEnv)->CallIntMethod);
  java_call_functions[swarm_type_ulong] =
      FFI_FN ((*(JNIEnv *)jEnv)->CallLongMethod);
  java_call_functions[swarm_type_slong] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallLongMethod);
  java_call_functions[swarm_type_float] =
      FFI_FN ((*(JNIEnv *)jEnv)->CallFloatMethod);
  java_call_functions[swarm_type_double] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallDoubleMethod);
  java_call_functions[swarm_type_pointer] = NULL;
  java_call_functions[swarm_type_string] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallObjectMethod);
  java_call_functions[swarm_type_jobject] = 
      FFI_FN ((*(JNIEnv *)jEnv)->CallObjectMethod);
}
#endif

@implementation FCall

+ createBegin: aZone
{
  FCall *newCall;
  newCall = [aZone allocIVars: self];
  newCall->args = NULL;
  return newCall;
}

- setArguments: arguments
{
  self->args = arguments;
  return self;
}

- getArguments
{
  return args;
}

- setFunction: (void (*)())fn
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
  ffunction = FFI_FN (get_imp ((Class) fclass, (SEL) fmethod));
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

void 
fillHiddenArguments (FCall * self)
{
  switch (self->callType)
    {
    case objccall: 
      ((FArguments *)self->args)->hiddenArguments = 2;	
      ((FArguments *)self->args)->argTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      ((FArguments *)self->args)->argValues[MAX_HIDDEN - 2] = &self->fobject;
      ((FArguments *)self->args)->argTypes[MAX_HIDDEN - 1] = &ffi_type_pointer;
      ((FArguments *)self->args)->argValues[MAX_HIDDEN - 1] = &self->fmethod;
      break;
#ifdef HAVE_JDK
    case javacall:
      ((FArguments *)self->args)->hiddenArguments = 3;
      ((FArguments *)self->args)->argTypes[MAX_HIDDEN - 3] = &ffi_type_pointer;
      ((FArguments *)self->args)->argValues[MAX_HIDDEN - 3] = &jniEnv;
      ((FArguments *)self->args)->argTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      ((FArguments *)self->args)->argValues[MAX_HIDDEN - 2] = &self->fobject;
      ((FArguments *)self->args)->argTypes[MAX_HIDDEN - 1] = &ffi_type_pointer;
      ((FArguments *)self->args)->argValues[MAX_HIDDEN - 1] = &self->fmethod;
      break;
    case javastaticcall:
      ((FArguments *)self->args)->hiddenArguments = 3;
      ((FArguments *)self->args)->argTypes[MAX_HIDDEN - 3] = &ffi_type_pointer;
      ((FArguments *)self->args)->argValues[MAX_HIDDEN - 3] = &jniEnv;
      ((FArguments *)self->args)->argTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      ((FArguments *)self->args)->argValues[MAX_HIDDEN - 2] = &self->fclass;
      ((FArguments *)self->args)->argTypes[MAX_HIDDEN - 1] = &ffi_type_pointer;
      ((FArguments *)self->args)->argValues[MAX_HIDDEN - 1] = &self->fmethod;
      break;
#endif
    }
}


- createEnd
{
  unsigned int res;
  if (_obj_debug && (callType == ccall || callType == objccall) && !ffunction)
    raiseEvent (SourceMessage, "Function to be called not set!\n");
  if (_obj_debug && !args)
    raiseEvent (SourceMessage, "Arguments and return type not specified!\n");
#ifdef HAVE_JDK
  if (callType == javacall || callType == javastaticcall)
      {
        ffunction = (callType == javacall ? 
                     java_call_functions[(unsigned) args->returnType] :
                     java_static_call_functions[(unsigned) args->returnType]);
        
        (jmethodID) fmethod = (callType == javacall ?
                               (*jniEnv)->GetMethodID (jniEnv, fclass, 
                                                       methodName, 
                                                       args->javaSignature) :
                               (*jniEnv)->GetStaticMethodID (jniEnv, fclass, 
                                                             methodName, 
                                                             args->javaSignature)); 
        if (!fmethod)
          raiseEvent (SourceMessage, "Could not find Java method!\n");
      }
#endif
  fillHiddenArguments (self);
  switch_to_ffi_types ((FArguments *) args);
  res = ffi_prep_cif (&cif, FFI_DEFAULT_ABI, 
		      args->hiddenArguments + args->assignedArguments, 
		      (ffi_type *) args->returnType, 
		      (ffi_type **) args->argTypes + MAX_HIDDEN - 
		      args->hiddenArguments);
  if (_obj_debug && res != FFI_OK)
    raiseEvent (SourceMessage,
                "Failed while preparing foreign function call closure!\n"); 
  return self;
}

- (void)_performAction_: anActvity
{
  ffi_call(&cif, ffunction, args->result, args->argValues + 
	   MAX_HIDDEN - args->hiddenArguments);  
}

- (void *)getResult
{
  return [args getResult];
}

@end




