// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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
#import <defobj/defalloc.h>

#include <swarmconfig.h>
#ifndef USE_AVCALL
#undef PACKAGE
#undef VERSION
#include <ffi.h>
#undef PACKAGE
#undef VERSION
#else
#include "fcall_objc.h"
#ifdef HAVE_JDK
#include "fcall_java.h"
#endif
#endif

#include <misc.h>
#include <objc/mframe.h>

#import <defobj/directory.h>

#ifdef HAVE_JDK
#include "java.h" // swarm_directory_java_ensure_objc
JNIEnv *jniEnv;
#include <javavars.h>
#endif

#import "COM.h"

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
  java_static_call_functions[fcall_type_boolean] =
      FUNCPTR ((*(JNIEnv *) jEnv)->CallStaticBooleanMethod);
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
  java_call_functions[fcall_type_boolean] = 
      FUNCPTR ((*(JNIEnv *) jEnv)->CallBooleanMethod);
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
    case ccall:
      break;
    case COMcall:
    case JScall:
      break;
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
      java_setup_call (fargs, self->fobject, self->fmethod);
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
      java_setup_static_call (fargs, self->fclass, self->fmethod);
#else
      fargs->ffiArgTypes[MAX_HIDDEN - 3] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 3] = &jniEnv;
      fargs->ffiArgTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 2] = &self->fclass;
      fargs->ffiArgTypes[MAX_HIDDEN - 1] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 1] = &self->fmethod;
#endif
      break;
#else
    case javacall: case javastaticcall:
      abort ();
      break;
#endif
    }

}

#ifndef USE_AVCALL
static ffi_type *ffi_types[FCALL_TYPE_COUNT] = {
  &ffi_type_void,
  &ffi_type_uchar, /* boolean */
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

  if (fc->callType == COMcall)
    {
      fc->COM_args = COM_create_arg_vector (fa->assignedArgumentCount + 1);
      
      for (i = 0; i < fa->assignedArgumentCount; i++)
        {
          unsigned pos = i + MAX_HIDDEN;
          
          COM_set_arg (fc->COM_args, i, fa->argTypes[pos], fa->argValues[pos]);
        }
      COM_set_return (fc->COM_args,
                      fa->assignedArgumentCount,
                      fa->returnType,
                      &fa->resultVal);
    }
  else if (fc->callType == JScall)
    {
      fc->COM_args = JS_create_arg_vector (fa->assignedArgumentCount + 1);
      
      for (i = 0; i < fa->assignedArgumentCount; i++)
        {
          unsigned pos = i + MAX_HIDDEN;
          
          JS_set_arg (fc->COM_args, i, fa->argTypes[pos], fa->argValues[pos]);
        }
      JS_set_return (fc->COM_args,
                     fa->assignedArgumentCount,
                     fa->returnType,
                     &fa->resultVal);
    }
  else
    {
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
          java_set_return_type (fc);
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
          objc_set_return_type (fc);
          fillHiddenArguments (fc);
          for (i = 0; i < fa->assignedArgumentCount; i++)
            {
              unsigned pos = i + MAX_HIDDEN;
              
              objc_add_primitive (fa, fa->argTypes[pos], fa->argValues[pos]);
            }
        }
#endif
    }
}


@implementation FCall_c

PHASE(Creating)
+ createBegin: aZone
{
  FCall_c *newCall = [aZone allocIVars: self];
  newCall->fargs = NULL;
  return newCall;
}

- setArguments: arguments
{
  fargs = arguments;
  return self;
}

- setFunctionPointer: (func_t)fn
{
  callType = ccall;
  ffunction = fn;
  return self;
}

- setMethod: (SEL)sel inObject: obj
{
  COMselector cSel;

  if (swarmDirectory && (cSel = SD_COM_FIND_SELECTOR_COM (sel)))
    {
      callType = COM_selector_is_javascript (cSel) ? JScall : COMcall;
      (COMselector) fmethod = cSel;
    }
  else
    {
      Class class;
  
      callType = objccall;
      fobject = obj;
      (SEL) fmethod = sel;
      class = getClass (obj);
      fclass = class;
      ffunction = FUNCPTR (get_imp ((Class) fclass, (SEL) fmethod));
    }
  return self;
}

- _setJavaMethod_: (const char *)theMethodName inObject: (JOBJECT)jObj
{
#ifdef HAVE_JDK
  jclass lref;
  callType = javacall;
  if (fclass)
    (*jniEnv)->DeleteGlobalRef (jniEnv, fclass);
  lref = (*jniEnv)->GetObjectClass (jniEnv, (jobject) jObj);
  fclass = (*jniEnv)->NewGlobalRef (jniEnv, lref);
  (*jniEnv)->DeleteLocalRef (jniEnv, lref);
  methodName = STRDUP (theMethodName);
  fobject = jObj;
#else
  java_not_available ();
#endif
  return self;
}    

- setJavaMethod: (const char *)theMethodName inObject: (JOBJECT)jObj
{
#ifdef HAVE_JDK
  [self _setJavaMethod_: theMethodName
        inObject: (*jniEnv)->NewGlobalRef (jniEnv, jObj)];
  fobjectPendingGlobalRefFlag = YES;
#else
  java_not_available ();
#endif
  return self;
}

- setJavaMethod: (const char *)theMethodName inClass: (const char *)className
{ 
#ifdef HAVE_JDK
  jclass lref;

  callType = javastaticcall;
  if (fclass)
    (*jniEnv)->DeleteGlobalRef (jniEnv, fclass);
  lref = (*jniEnv)->FindClass (jniEnv, className);
  fclass = (*jniEnv)->NewGlobalRef (jniEnv, lref);
  (*jniEnv)->DeleteLocalRef (jniEnv, lref);
  methodName = STRDUP (theMethodName);
#else
  java_not_available ();
#endif
  return self;
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
      (*jniEnv)->ExceptionClear (jniEnv);

      if (callType == javacall)
        {
          fmethod =
            (*jniEnv)->GetMethodID (jniEnv,
                                    fclass,
                                    methodName, 
                                    fargs->javaSignature);
          ffunction = java_call_functions[fargs->returnType];
        }
      else
        {
          fmethod = (*jniEnv)->GetStaticMethodID (jniEnv,
                                                  fclass,
                                                  methodName, 
                                                  fargs->javaSignature);
          ffunction = java_static_call_functions[fargs->returnType];
        }
      if (!fmethod)
        raiseEvent (SourceMessage,
                    "Could not find Java method: `%s' `%s'\n",
                    methodName, fargs->javaSignature);
    }
#endif
  add_ffi_types (self);
#ifndef USE_AVCALL
  if (callType != COMcall && callType != JScall)
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
                    "Failed while preparing foreign function call\n"); 
    }
#endif
  setMappedAlloc (self);
  setNextPhase (self);
  return self;
}

+ create: aZone target: target
                selector: (SEL)aSel
                arguments: (id <FArguments>)fa
{
  id <FCall> fc = [FCall createBegin: aZone];

  [fc setArguments: fa];
#ifdef HAVE_JDK
  if ([fa getLanguage] == LanguageJava)
    {
      jstring string;
      const char *javaMethodName;
      jboolean copy;
      jobject jsel = SD_JAVA_FIND_SELECTOR_JAVA (aSel);
      jobject jobj = SD_JAVA_FIND_OBJECT_JAVA (target);
      
      string = (*jniEnv)->GetObjectField (jniEnv, jsel, f_nameFid);
      javaMethodName = (*jniEnv)->GetStringUTFChars (jniEnv, string, &copy);

      [(id) fc _setJavaMethod_: javaMethodName inObject: jobj];
      if (copy)
        (*jniEnv)->ReleaseStringUTFChars (jniEnv, string, javaMethodName);
      (*jniEnv)->DeleteLocalRef (jniEnv, string);
    }
  else
#endif
    [fc setMethod: aSel inObject: target];

  return [fc createEnd];
}

void
updateTarget (FCall_c *self, id target)
{
#ifdef HAVE_JDK
  if ([target respondsTo: M(isJavaProxy)])
    updateJavaTarget (self, SD_JAVA_FIND_OBJECT_JAVA (target));
  else
#endif
    self->fobject = target;
  add_ffi_types (self);
}

#ifdef HAVE_JDK
void
updateJavaTarget (FCall_c *self, JOBJECT target)
{
  if (self->fobjectPendingGlobalRefFlag)
    {
      (*jniEnv)->DeleteGlobalRef (jniEnv, (jobject) self->fobject);
      self->fobjectPendingGlobalRefFlag = NO;
    }
  self->fobject = target;
  add_ffi_types (self);
}
#endif

PHASE(Using)

- (call_t)getCallType
{
  return callType;
}

- getArguments
{
  return fargs;
}

- (void)performCall
{
#ifdef HAVE_JDK
  if (fargs->pendingGlobalRefFlag)
    {
      (*jniEnv)->DeleteGlobalRef (jniEnv, (jobject) fargs->resultVal.object);
      fargs->pendingGlobalRefFlag = NO;
    }
#endif
  if (callType == COMcall)
    COM_selector_invoke ((COMselector) fmethod, COM_args);
  else if (callType == JScall)
    JS_selector_invoke ((COMselector) fmethod, COM_args);
#ifndef USE_AVCALL
  else
    {
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
        case fcall_type_boolean:
          fargs->resultVal.boolean = VAL (BOOL, ret);
          break;
        case fcall_type_schar:
          // character return is broken in libffi-1.18
          fargs->resultVal.schar = VAL (int, ret);
          break;
        case fcall_type_uchar:
          // character return is broken in libffi-1.18
          fargs->resultVal.uchar = VAL (unsigned, ret);
          break;
        case fcall_type_sint:
          fargs->resultVal.sint = VAL (int, ret);
          break;
        case fcall_type_uint:
          fargs->resultVal.uint = VAL (unsigned, ret);
          break;
        case fcall_type_sshort:
          // short return is broken in libffi-1.18
          fargs->resultVal.sshort = VAL (int, ret);
          break;
        case fcall_type_ushort:
          // short return is broken in libffi-1.18
          fargs->resultVal.ushort = VAL (unsigned, ret);
          break;
        case fcall_type_slong:
          fargs->resultVal.slong = VAL (long, ret);
          break;
        case fcall_type_ulong:
          fargs->resultVal.ulong = VAL (unsigned long, ret);
          break;
        case fcall_type_slonglong:
          fargs->resultVal.slonglong = VAL (long long, ret);
          break;
        case fcall_type_ulonglong:
          fargs->resultVal.ulonglong = VAL (unsigned long long, ret);
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
          fargs->resultVal.object = VAL (id, ret);
          break;
        case fcall_type_class:
          fargs->resultVal._class = VAL (Class, ret);
          break;
        case fcall_type_selector:
          fargs->resultVal.selector = VAL (SEL, ret);
          break;
        case fcall_type_string:
          fargs->resultVal.string = VAL (const char *, ret);
          break;
#ifdef HAVE_JDK
        case fcall_type_jobject:
          fargs->resultVal.object = (id) VAL (jobject, ret);
          break;
        case fcall_type_jstring:
          fargs->resultVal.object = (id) VAL (jstring, ret);
          break;
#endif
        default:
          abort ();
        }
    }
#else
#ifdef HAVE_JDK
  else if (callType == javacall || callType == javastaticcall)
    java_call (fargs);
  else 
#endif
    objc_call (fargs);
#endif
#ifdef HAVE_JDK
  if (callType == javacall || callType == javastaticcall)
    {
      jobject exception;

      if ((exception = (*jniEnv)->ExceptionOccurred (jniEnv)))
        {
          (*jniEnv)->ExceptionDescribe (jniEnv);
          (*jniEnv)->ExceptionClear (jniEnv);
          (*jniEnv)->DeleteLocalRef (jniEnv, exception);

          raiseEvent (InvalidOperation,
                      "Exception occurred in Java method `%s'\n", methodName);
        }
      if (fargs->returnType == fcall_type_jobject
	  || fargs->returnType == fcall_type_jstring)
	{
	  jobject lref = (jobject) fargs->resultVal.object;
	  
	  fargs->resultVal.object =
	    (id) (*jniEnv)->NewGlobalRef (jniEnv, lref);
          (*jniEnv)->DeleteLocalRef (jniEnv, lref);
	  fargs->pendingGlobalRefFlag = YES;
	}
    }
#endif
}

- (void *)getResult
{
  return [fargs getResult];
}

- (retval_t)getRetVal: (retval_t)retVal buf: (types_t *)buf
{
#ifdef HAVE_JDK
  id return_jobject (void)
    {
      return SD_JAVA_ENSURE_OBJECT_OBJC ((jobject) buf->object);
    }
  const char *return_jstring (void)
    {
      const char *newString = JAVA_COPY_STRING ((jstring) buf->object);
      
      (void) SD_JAVA_SWITCHOBJC ((jstring) buf->object,
                                 (id) newString);
      return newString;
    }
#endif

#if defined(__i386__) && (__GNUC__ == 2) && (__GNUC_MINOR__ < 95)
#define BUGGY_BUILTIN_APPLY
#endif

  *buf = ((FArguments_c *) fargs)->resultVal;

#ifndef BUGGY_BUILTIN_APPLY
  {
    BOOL return_boolean (void) { return buf->boolean; }
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
    Class return_class (void) { return buf->_class; }
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
      case fcall_type_boolean:
        APPLY (return_boolean);
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
      case fcall_type_boolean:
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
        ptr = &buf->_class;
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

- (func_t)getFunctionPointer
{
  if (callType == ccall)
    return ffunction;
  else
    abort ();
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
}

- (void)dropAllocations: (BOOL)componentAlloc
{
  if (callType == COMcall)
    COM_free_arg_vector (COM_args);
  else if (callType == JScall)
    JS_free_arg_vector (COM_args);
#ifdef HAVE_JDK
  else if (callType == javacall)
    {
      if (fobjectPendingGlobalRefFlag)
        (*jniEnv)->DeleteGlobalRef (jniEnv, fobject);
      (*jniEnv)->DeleteGlobalRef (jniEnv, fclass);
    }
  else if (callType == javastaticcall)
    (*jniEnv)->DeleteGlobalRef (jniEnv, fclass);
#endif
  if (methodName)
    FREEBLOCK (methodName);
  [super dropAllocations: componentAlloc];
}

- (void)drop
{
  [self dropAllocations: YES];
}
@end
