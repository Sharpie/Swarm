// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarm.h> // initSwarm, swarmGUIMode
#import <defobj.h> // defobj_java_call_init_tables
#include <swarmconfig.h> // HAVE_KAFFE
#import "../../src/defobj/java.h" 
#import <defobj/javavars.h>

JNIEXPORT void JNICALL
Java_swarm_SwarmEnvironmentImpl_initSwarm (JNIEnv *env,
                                           jobject obj,
                                           jstring appName,
                                           jstring version,
                                           jstring bugAddress,
                                           jobjectArray args)
{
  JNIEXPORT void JNICALL Java_swarm_SwarmEnvironmentImpl__initSwarm___Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2_3Ljava_lang_String_2 (JNIEnv *env, jobject obj, jstring appName, jstring version, jstring bugAddress, jobjectArray args);
  jobject swarmEnvironment = (*jniEnv)->NewGlobalRef (jniEnv, obj);

  // (*jniEnv)->DeleteLocalRef (jniEnv, obj);
  defobj_init_java_call_tables ((void *) env);
#ifdef hpux
  {
#ifdef HAVE_KAFFE
    extern void libkaffeswarmstubs_constructor (void);
    extern void libkaffeswarm_constructor (void);

    libkaffeswarmstubs_constructor ();
    libkaffeswarm_constructor ();
#else
    extern void libjavaswarmstubs_constructor (void);
    extern void libjavaswarm_constructor (void);

    libjavaswarmstubs_constructor ();
    libjavaswarm_constructor ();
#endif
  }
#endif

  Java_swarm_SwarmEnvironmentImpl__initSwarm___Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2_3Ljava_lang_String_2 (env, swarmEnvironment, appName, version, bugAddress, args);

  swarm_directory_java_associate_objects (swarmEnvironment);

  {
    jfieldID fid;
    
    if (!(fid = (*env)->GetFieldID (env, c_SwarmEnvironmentImpl, "guiFlag", "Z")))
      abort ();
    
    (*env)->SetBooleanField (env, swarmEnvironment, fid, (jboolean) swarmGUIMode);
  }
}
