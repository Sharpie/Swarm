// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include "JavaProxy.h"
#import <defobj/directory.h>
#import <swarmconfig.h>
#ifdef HAVE_JDK
#import "java.h"
#endif

@implementation JavaProxy

- (BOOL)isJavaProxy
{
  return YES;
}

- createJavaCounterpart: (const char *)typeName
{
  [self createEnd];
#ifdef HAVE_JDK
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
                                     SD_JAVA_ENSUREJAVA ([self getZone]));
      else
        {
          (*jniEnv)->ExceptionClear (jniEnv);
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
      SD_JAVA_ADD (jobj, self);
      
      (*jniEnv)->DeleteLocalRef (jniEnv, jobj);
      (*jniEnv)->DeleteLocalRef (jniEnv, class);
    }
  }
#endif
  return self;
}

@end
