#include "JavaProxy.h"
#import <defobj/directory.h>

@implementation JavaProxy

- (BOOL)isJavaProxy
{
  return YES;
}

- createJavaCounterpart: (const char *)typeName
{
  [self createEnd];
  {
    jclass class = (*jniEnv)->FindClass (jniEnv, typeName);
    
    if (!class)
      raiseEvent (SourceMessage,
                  "Could not find Java class `%s'\n", 
                  typeName);
    {
      jmethodID method =
        (*jniEnv)->GetMethodID (jniEnv, class, "<init>", "()V");
      
      jobject jobj = 
        (*jniEnv)->NewObject (jniEnv, class, method);
      
      if (!jobj)
        abort ();
      SD_ADD (jniEnv, jobj, self);
      
      (*jniEnv)->DeleteLocalRef (jniEnv, jobj);
      (*jniEnv)->DeleteLocalRef (jniEnv, class);
    }
  }
  return self;
}

@end
