// include first to avoid Kaffe jmalloc/stdlib conflict
#include <misc.h> // xmalloc, strdup
#import <simtools.h> // initSwarm
#import <defobj.h> // defobj_java_call_init_tables
#import "directory.h" // java_directory_init
#include <jni.h>

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentSwarm (JNIEnv *env, jobject obj)
{
  return JENSUREJAVA (env, getCurrentSwarm());
}

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentSchedule (JNIEnv *env, jobject obj)
{
  return JENSUREJAVA (env, getCurrentSchedule());
}

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentSwarmActivity (JNIEnv *env, jobject obj)
{
  return JENSUREJAVA (env, getCurrentSwarmActivity ());
}

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentScheduleActivity (JNIEnv *env, 
							jobject obj)
{
  return JENSUREJAVA (env, getCurrentScheduleActivity());
}


JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentOwnerActivity (JNIEnv *env, jobject obj)
{
  return JENSUREJAVA (env, getCurrentOwnerActivity ());
}

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentAction (JNIEnv *env, jobject obj)
{
  return JENSUREJAVA (env, getCurrentAction());
}


JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentActivity (JNIEnv *env, jobject obj)
{
  return JENSUREJAVA (env, getCurrentActivity());
}


JNIEXPORT int JNICALL
Java_swarm_SwarmEnvironment_getCurrentTime (JNIEnv *env, jobject obj)
{
  return getCurrentTime();
}


JNIEXPORT void JNICALL
Java_swarm_SwarmEnvironment_initSwarm (JNIEnv *env,
                                       jobject obj,
                                       jobjectArray args)
{
  int i = 0;
  const char **argv;
  int argc;
  const char *utf;
  jstring jstr;
  jboolean isCopy;

  argc = (*env)->GetArrayLength (env, args) + 1;   
  argv = (const char **) xmalloc (sizeof (const char *) * argc);

  argv[0] = "javaswarm";
  for (i = 0; i < argc - 1; i++)
    {
      jstr = (*env)->GetObjectArrayElement (env, args, i);
      utf = (const char *) (*env)->GetStringUTFChars (env, jstr, &isCopy);
      argv[i + 1] = isCopy ? (const char *) utf : strdup (utf);
    }

  defobj_init_java_call_tables ((void *) env);
#define APPNAME "javaswarm"
  initSwarmApp (argc, argv, VERSION, "bug-swarm@santafe.edu");
#ifdef hpux
  {
    extern void libjavaswarmstubs_constructor (void);
    extern void libjavaswarm_constructor (void);

    libjavaswarmstubs_constructor ();
    libjavaswarm_constructor ();
  }
#endif
  java_directory_init (env, obj);
}

