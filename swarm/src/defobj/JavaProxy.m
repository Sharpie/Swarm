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
      jobject jobj;
      jmethodID method =
        (*jniEnv)->GetMethodID (jniEnv, class, "<init>", "(Lswarm/defobj/Zone;)V");
      
      if (method)
        jobj = (*jniEnv)->NewObject (jniEnv, class, method,
                                     SD_ENSUREJAVA (jniEnv, [self getZone]));
      else
        {
          method = (*jniEnv)->GetMethodID (jniEnv, class, "<init>", "()V");
          
          if (!method)
            raiseEvent (SourceMessage, "Could not find constructor for `%s'\n",
                        typeName);
          
          {
            jobj = (*jniEnv)->NewObject (jniEnv, class, method);

            if (!jobj)
              abort ();
          }
        }
      SD_ADD (jniEnv, jobj, self);
      
      (*jniEnv)->DeleteLocalRef (jniEnv, jobj);
      (*jniEnv)->DeleteLocalRef (jniEnv, class);
    }
  }
  return self;
}

@end
