// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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

/*
Name:         FCall.m
Description:  foreign function call
Library:      defobj
*/

#import <defobj/FCall.h>
#import <defobj.h>
#import <defobj/FArguments.h>
#import <defobj/swarm-objc-api.h>
#import <defobj/defalloc.h>
#import <defobj/macros.h>

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
#ifdef GNUSTEP
#include <Foundation/NSMethodSignature.h>
// mframe internals not available
#define BUGGY_BUILTIN_APPLY
#else
#ifdef USE_MFRAME
#include <objc/mframe.h>
#endif
#endif

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

#define GCINFO_SIZE (2 * sizeof (void *))
#define gc_fclass gcInfo[0]
#define gc_fobject gcInfo[1]

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
  java_static_call_functions[fcall_type_jselector] = 
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
  java_call_functions[fcall_type_jselector] = 
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
      objc_setup_call (fargs, self->gc_fobject, self->fmethod);
#else
      fargs->ffiArgTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 2] = &self->gc_fobject;
      fargs->ffiArgTypes[MAX_HIDDEN - 1] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 1] = &self->fmethod;
#endif
      break;
#ifdef HAVE_JDK
    case javacall:
      fargs->hiddenArgumentCount = 3;
#ifdef USE_AVCALL
      java_setup_call (fargs, self->gc_fobject, self->fmethod);
#else
      fargs->ffiArgTypes[MAX_HIDDEN - 3] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 3] = &jniEnv;
      fargs->ffiArgTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 2] = &self->gc_fobject;
      fargs->ffiArgTypes[MAX_HIDDEN - 1] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 1] = &self->fmethod;
#endif
      break;
    case javastaticcall:
      fargs->hiddenArgumentCount = 3;
#ifdef USE_AVCALL
      java_setup_static_call (fargs, self->gc_fclass, self->fmethod);
#else
      fargs->ffiArgTypes[MAX_HIDDEN - 3] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 3] = &jniEnv;
      fargs->ffiArgTypes[MAX_HIDDEN - 2] = &ffi_type_pointer;
      fargs->argValues[MAX_HIDDEN - 2] = &self->gc_fclass;
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
  &ffi_type_pointer,
  &ffi_type_pointer,
  &ffi_type_pointer 
};
#endif

static void
add_ffi_types (FCall_c *fc)
{
  unsigned i;
  FArguments_c *fa = fc->fargs;

  if (fc->callType == COMcall)
    {
      fc->COM_params = COM_create_params (fa->assignedArgumentCount + 1);
      
      for (i = 0; i < fa->assignedArgumentCount; i++)
        {
          unsigned pos = i + MAX_HIDDEN;
          val_t val = { fa->argTypes[pos], *(types_t *) fa->argValues[pos] };
          
          COM_set_arg (fc->COM_params, i, &val);
        }
      COM_set_return (fc->COM_params,
                      fa->assignedArgumentCount,
                      &fa->retVal);
    }
  else if (fc->callType == JScall)
    {
      fc->COM_params = JS_create_params (fa->assignedArgumentCount + 1);
      
      for (i = 0; i < fa->assignedArgumentCount; i++)
        {
          unsigned pos = i + MAX_HIDDEN;
          val_t val = { fa->argTypes[pos], *(types_t *) fa->argValues[pos] };
          
          JS_set_arg (fc->COM_params, i, &val);
        }
    }
  else
    {
#ifndef USE_AVCALL
      fa->ffiReturnType = ffi_types[fa->retVal.type];
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

#define UPDATEMETHODNAME(value) { if (methodName) FREEBLOCK (methodName); methodName = STRDUP (value); }

@implementation FCall_c

PHASE(Creating)
+ createBegin: aZone
{
  FCall_c *newCall = [aZone allocIVars: self];
  newCall->fargs = NULL;
  {
    JOBJECT *ptr = [_obj_GCFixedRootZone allocBlock: GCINFO_SIZE];
    
    memset (ptr, 0, GCINFO_SIZE);
    newCall->gcInfo = ptr;
  }
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

- _setJavaMethod_: (const char *)theMethodName inObject: (JOBJECT)jObj
{
#ifdef HAVE_JDK
  jclass lref;
  char buf[strlen (theMethodName) + 1], *p;

  callType = javacall;

  
  strcpy (buf, theMethodName);
  p = strchr (buf, ':');
  if (p)
    *p = '\0';
  
  if (gc_fclass)
    (*jniEnv)->DeleteGlobalRef (jniEnv, gc_fclass);
  lref = (*jniEnv)->GetObjectClass (jniEnv, (jobject) jObj);
  gc_fclass = (*jniEnv)->NewGlobalRef (jniEnv, lref);
  (*jniEnv)->DeleteLocalRef (jniEnv, lref);
  UPDATEMETHODNAME (buf);
  gc_fobject = jObj;
#else
  java_not_available ();
#endif
  return self;
}    

- setMethodFromName: (const char *)theMethodName inObject: obj
{
  COMobject cObj = SD_COM_FIND_OBJECT_COM (obj);

  if (cObj)
    {
      if (COM_is_javascript (cObj))
        {
          gc_fobject = SD_COM_FIND_OBJECT_COM (obj);
          callType = JScall;
	  UPDATEMETHODNAME (theMethodName);
          return self;
        }
    }
  else 
    {
#if SWARM_OBJC_DONE
      SEL sel = sel_get_any_typed_uid (theMethodName);
#else
      SEL sel = swarm_sel_getUidWithType (theMethodName);
#endif

#ifdef HAVE_JDK
      if (!sel)
        {
          jobject jObj = SD_JAVA_FIND_OBJECT_JAVA (obj);
          
          if (jObj)
            {
              [self setJavaMethodFromName: theMethodName inObject: jObj];
              return self;
            }
        }
#endif
      [self setMethodFromSelector: sel inObject: obj];
    }
  return self;
}

- setMethodFromSelector: (SEL)sel inObject: obj
{
  COMselector cSel = SD_COM_FIND_SELECTOR_COM (sel);
  
  if (cSel)
    {
      callType = COM_selector_is_javascript (cSel) ? JScall : COMcall;
       fmethod = COM_selector_method (cSel);
       gc_fobject = SD_COM_FIND_OBJECT_COM (obj);
      
      if (callType == JScall)
        UPDATEMETHODNAME (swarm_sel_getName (sel));
      return self;
    }
#ifdef HAVE_JDK
  else if ([fargs getLanguage] == LanguageJava)
    {
      jobject jsel = SD_JAVA_FIND_SELECTOR_JAVA (sel);

      if (jsel)
        {
          jstring string;
          const char *javaMethodName;
          jboolean copy;
          jobject jobj = SD_JAVA_FIND_OBJECT_JAVA (obj);
          
          string = (*jniEnv)->GetObjectField (jniEnv, jsel, f_nameFid);
          javaMethodName =
            (*jniEnv)->GetStringUTFChars (jniEnv, string, &copy);
          
          [(id) self setJavaMethodFromName: javaMethodName inObject: jobj];
          if (copy)
            (*jniEnv)->ReleaseStringUTFChars (jniEnv, string,
                                              javaMethodName);
          (*jniEnv)->DeleteLocalRef (jniEnv, string);
          return self;
        }
    }
#endif
  {
    Class class;
    
    callType = objccall;
    gc_fobject = obj;
    fmethod = (void *)sel;
    class = getClass (obj);
    gc_fclass = class;
    ffunction = FUNCPTR (swarm_class_getMethodImplementation ((Class) gc_fclass, (SEL) fmethod));
    return self;
  }
}

- setJavaMethodFromName: (const char *)theMethodName inObject: (JOBJECT)jObj
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

- setJavaMethodFromName: (const char *)theMethodName inClass: (const char *)className
{ 
#ifdef HAVE_JDK
  jclass lref;

  callType = javastaticcall;
  if (gc_fclass)
    (*jniEnv)->DeleteGlobalRef (jniEnv, gc_fclass);
  lref = (*jniEnv)->FindClass (jniEnv, className);
  gc_fclass = (*jniEnv)->NewGlobalRef (jniEnv, lref);
  (*jniEnv)->DeleteLocalRef (jniEnv, lref);
  UPDATEMETHODNAME (theMethodName);
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
                                    gc_fclass,
                                    methodName, 
                                    fargs->javaSignature);
          ffunction = java_call_functions[fargs->retVal.type];
        }
      else
        {
          fmethod = (*jniEnv)->GetStaticMethodID (jniEnv,
                                                  gc_fclass,
                                                  methodName, 
                                                  fargs->javaSignature);
          ffunction = java_static_call_functions[fargs->retVal.type];
        }
      if (!fmethod)
        {
          (*jniEnv)->ExceptionClear (jniEnv);
          [self drop];
          return nil;
        }
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
  [fc setMethodFromSelector: aSel inObject: target];

  return [fc createEnd];
}

+ create: aZone target: target
            methodName: (const char *)theMethodName
             arguments: (id <FArguments>)fa
{
  id <FCall> fc = [FCall createBegin: aZone];

  [fc setArguments: fa];
  [fc setMethodFromName: theMethodName inObject: target];

  return [fc createEnd];
}

void
updateTarget (FCall_c *self, id target)
{
  if (self->fargs->language == LanguageObjc)
    {
      self->gc_fobject = target;
      add_ffi_types (self);
    }
#ifdef HAVE_JDK
  else if (self->fargs->language == LanguageJava)
    {
      jobject jObj = SD_JAVA_FIND_OBJECT_JAVA (target);

      updateJavaTarget (self, jObj);
    }
#endif
  else if (self->fargs->language == LanguageCOM)
    {
      COMobject cObj = SD_COM_FIND_OBJECT_COM (target);

       self->gc_fobject = cObj;
      add_ffi_types (self);
    }
}

#ifdef HAVE_JDK
void
updateJavaTarget (FCall_c *self, JOBJECT target)
{
  if (self->fobjectPendingGlobalRefFlag)
    {
      (*jniEnv)->DeleteGlobalRef (jniEnv, (jobject) self->gc_fobject);
      self->fobjectPendingGlobalRefFlag = NO;
    }
  self->gc_fobject = target;
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
      (*jniEnv)->DeleteGlobalRef (jniEnv,
                                  (jobject) fargs->retVal.val.object);
      fargs->pendingGlobalRefFlag = NO;
    }
#endif
  if (callType == COMcall)
    COM_method_invoke ((COMmethod) fmethod,
                       (COMobject) gc_fobject,
                       COM_params);
  else if (callType == JScall)
    {
      JS_method_invoke ((COMobject) gc_fobject,
                        methodName,
                        COM_params);
      JS_set_return (COM_params,
                     fargs->assignedArgumentCount,
                     &fargs->retVal);
    }
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
      
      switch (fargs->retVal.type)
        {
        case fcall_type_void:
          break;
        case fcall_type_boolean:
          fargs->retVal.val.boolean = VAL (BOOL, ret);
          break;
        case fcall_type_schar:
          // character return is broken in libffi-1.18
          fargs->retVal.val.schar = VAL (int, ret);
          break;
        case fcall_type_uchar:
          // character return is broken in libffi-1.18
          fargs->retVal.val.uchar = VAL (unsigned, ret);
          break;
        case fcall_type_sint:
          fargs->retVal.val.sint = VAL (int, ret);
          break;
        case fcall_type_uint:
          fargs->retVal.val.uint = VAL (unsigned, ret);
          break;
        case fcall_type_sshort:
          // short return is broken in libffi-1.18
          fargs->retVal.val.sshort = VAL (int, ret);
          break;
        case fcall_type_ushort:
          // short return is broken in libffi-1.18
          fargs->retVal.val.ushort = VAL (unsigned, ret);
          break;
        case fcall_type_slong:
          fargs->retVal.val.slong = VAL (long, ret);
          break;
        case fcall_type_ulong:
          fargs->retVal.val.ulong = VAL (unsigned long, ret);
          break;
        case fcall_type_slonglong:
          fargs->retVal.val.slonglong = VAL (long long, ret);
          break;
        case fcall_type_ulonglong:
          fargs->retVal.val.ulonglong = VAL (unsigned long long, ret);
          break;
        case fcall_type_float:
          fargs->retVal.val._float = ret._float;
          break;
        case fcall_type_double:
          fargs->retVal.val._double = ret._double;
          break;
        case fcall_type_long_double:
          fargs->retVal.val._long_double = ret._long_double;
          break;
        case fcall_type_object:
          fargs->retVal.val.object = VAL (id, ret);
          break;
        case fcall_type_class:
          fargs->retVal.val._class = VAL (Class, ret);
          break;
        case fcall_type_selector:
          fargs->retVal.val.selector = VAL (SEL, ret);
          break;
        case fcall_type_string:
          fargs->retVal.val.string = VAL (const char *, ret);
          break;
#ifdef HAVE_JDK
        case fcall_type_jobject:
          fargs->retVal.val.object = (id) VAL (jobject, ret);
          break;
        case fcall_type_jstring:
          fargs->retVal.val.object = (id) VAL (jstring, ret);
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
      if (fargs->retVal.type == fcall_type_jobject
	  || fargs->retVal.type == fcall_type_jstring)
	{
	  jobject lref = (jobject) fargs->retVal.val.object;
	  
	  fargs->retVal.val.object =
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

#if (defined(__i386__) && ((__GNUC__ == 2) && ((__GNUC_MINOR__ < 95)  || (__GNUC_MINOR__ == 96))) || (__GNUC__ == 3 && defined(__i386__)) || (__GNUC__ == 4 && defined(__i386__)) || defined (__CYGWIN__))
// Let's see how things go now with >=  3.4 - mgd
#define BUGGY_BUILTIN_APPLY
#endif

  *buf = ((FArguments_c *) fargs)->retVal.val;

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
    
    switch (fargs->retVal.type)
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
    
    switch (fargs->retVal.type)
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
    COM_free_params (COM_params);
  else if (callType == JScall)
    JS_free_params (COM_params);
#ifdef HAVE_JDK
  else if (callType == javacall)
    {
      if (fobjectPendingGlobalRefFlag)
        (*jniEnv)->DeleteGlobalRef (jniEnv, gc_fobject);
      (*jniEnv)->DeleteGlobalRef (jniEnv, gc_fclass);
    }
  else if (callType == javastaticcall)
    (*jniEnv)->DeleteGlobalRef (jniEnv, gc_fclass);
#endif
  [_obj_GCFixedRootZone freeBlock: gcInfo blockSize: GCINFO_SIZE];

  if (methodName)
    FREEBLOCK (methodName);
  [super dropAllocations: componentAlloc];
}

- (void)drop
{
  [self dropAllocations: YES];
}
@end
