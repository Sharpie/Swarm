#include "JavaProxy.h"
#include <objc/objc-api.h>
#include <objc/mframe.h>

#import <defobj/FArguments.h>
#import <defobj.h> // FCall

#import "directory.h"

#include <jni.h>

@implementation JavaProxy

- (retval_t)forward: (SEL)aSel : (arglist_t)argFrame
{
  NSArgumentInfo info;
  FArguments *fa;
  id <FCall> fc;
  types_t val;
  id aZone = [self getZone];
  const char *type = sel_get_type (aSel);
  jobject jobj = JFINDJAVA (self);
  jobject jsel;
  
  if (!type)
    {
      aSel = sel_get_any_typed_uid (sel_get_name (aSel));
      type = sel_get_type (aSel);
      if (!type)
        abort ();
    }
  jsel = JFINDJAVA ((id) aSel);
  fa = [FArguments createBegin: aZone];
  type = mframe_next_arg (type, &info);
  mframe_get_arg (argFrame, &info, &val);
  [fa setObjCReturnType: *info.type];
  /* skip object and selector */
  type = mframe_next_arg (type, &info);
  type = mframe_next_arg (type, &info);
  while ((type = mframe_next_arg (type, &info)))
    {
      mframe_get_arg (argFrame, &info, &val);
      [fa addArgument: &val ofObjCType: *info.type];
    }
  fa = [fa createEnd];

  {
    jclass clazz = (*jniEnv)->GetObjectClass (jniEnv, jsel);
    jfieldID nameFid;
    jstring string;
    const char *methodName;
    jboolean copyFlag;
    
    if (!(nameFid = (*jniEnv)->GetFieldID (jniEnv, clazz,
                                         "signature",
                                         "Ljava/lang/String;")))
      abort ();
    string = (*jniEnv)->GetObjectField (jniEnv, jsel, nameFid);
    methodName = (*jniEnv)->GetStringUTFChars (jniEnv, string, &copyFlag);
   
    fc = [[[[FCall createBegin: aZone] setArguments: fa]
            setJavaMethod: methodName inObject: jobj] createEnd];
    if (copyFlag)
      (*jniEnv)->ReleaseStringUTFChars (jniEnv, string, methodName);
  }
  printf ("before call\n");
  [fc performCall];
  printf ("after call\n");
  {
    types_t typebuf;
    retval_t retval = [fc getRetVal: argFrame buf: &typebuf];

    [fc drop];
    [fa drop];
    return retval;
  }
}
@end
