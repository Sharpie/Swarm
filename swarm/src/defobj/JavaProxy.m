// Swarm library. Copyright � 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "JavaProxy.h"
#import "JavaCollection.h"
#import <defobj/directory.h>
#import <swarmconfig.h>
#ifdef HAVE_JDK
#import "java.h"
#import "javavars.h"
#endif
#import <defobj/defalloc.h>

@implementation JavaProxy
PHASE(Creating)
PHASE(Setting)
PHASE(Using)
- (BOOL)isJavaProxy
{
  return YES;
}

- (void)createJavaCounterpart: (const char *)typeName
{
  [self createEnd];
#ifdef HAVE_JDK
  {
    jclass class = java_find_class (typeName, YES);
    
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
                                     SD_JAVA_ENSURE_OBJECT_JAVA ([self getZone]));
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
              raiseEvent (SourceMessage, "Could not instantiate `%s'",
                          typeName);
          }
        }
      if ((*jniEnv)->IsInstanceOf (jniEnv, jobj, c_Collection))
        self->isa = [JavaCollection self];

      SD_JAVA_ADD_OBJECT (jobj, self);
      
      (*jniEnv)->DeleteLocalRef (jniEnv, jobj);
      (*jniEnv)->DeleteLocalRef (jniEnv, class);
    }
  }
#endif
}

- doesNotRecognize: (SEL)sel
{
  id fa = [FArguments createBegin: getCZone (getZone (self))];
  id fc;

  [fa setLanguage: LanguageJava];
  [fa addSelector: sel];
  [fa setReturnType: fcall_type_void];
  fa = [fa createEnd];

#ifdef HAVE_JDK
  {
    jobject jobj = SD_JAVA_FIND_OBJECT_JAVA (self);
    jobject jcls = (*jniEnv)->GetObjectClass (jniEnv, jobj);
    
    SD_JAVA_ENSURE_SELECTOR_JAVA (jcls, M(doesNotRecognize:));
    (*jniEnv)->DeleteLocalRef (jniEnv, jcls);
  }
#endif

  fc = [FCall create: getCZone (getZone (self))
              target: self
              selector: M(doesNotRecognize:)
              arguments: fa];

  if (fc)
    {
      [fc performCall];
      [fc drop];
      [fa drop];
    }
  else
    {
      [fa drop];
      [super doesNotRecognize: sel];
    }
  
  return self;
}

- (int)compare: obj2
{
  jobject jobj1 = SD_JAVA_FIND_OBJECT_JAVA (self);
  jobject jobj2 = ([obj2 isInstance]
                   ? SD_JAVA_ENSURE_OBJECT_JAVA (obj2)
                   : SD_JAVA_FIND_CLASS_JAVA (obj2));
  jmethodID method;
  jclass class;
  jint ret;

  if (!(class = (*jniEnv)->GetObjectClass (jniEnv, jobj1)))
    abort ();

  if (!(method = (*jniEnv)->GetMethodID (jniEnv,
					 class,
					 "compareTo", 
					 "(Ljava/lang/Object;)I")))
    {
      (*jniEnv)->ExceptionClear (jniEnv);
      ret = [super compare: obj2];
    }
  else
    ret = (*jniEnv)->CallIntMethod (jniEnv, jobj1, method, jobj2);
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
  return ret;
}

@end
