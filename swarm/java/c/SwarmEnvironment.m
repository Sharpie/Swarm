// include first to avoid Kaffe jmalloc/stdlib conflict
#include <misc.h> // xmalloc, strdup
#import <simtools.h> // initSwarm
#import <defobj.h> // defobj_java_call_init_tables
#import "directory.h" // java_directory_init
#import <simtoolsgui.h>
#include <jni.h>


JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentSwarm (JNIEnv *env, jobject obj)
{
  return SD_ENSUREJAVA (env, getCurrentSwarm());
}

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentSchedule (JNIEnv *env, jobject obj)
{
  return SD_ENSUREJAVA (env, getCurrentSchedule());
}

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentSwarmActivity (JNIEnv *env, jobject obj)
{
  return SD_ENSUREJAVA (env, getCurrentSwarmActivity ());
}

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentScheduleActivity (JNIEnv *env, 
							jobject obj)
{
  return SD_ENSUREJAVA (env, getCurrentScheduleActivity());
}


JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentOwnerActivity (JNIEnv *env, jobject obj)
{
  return SD_ENSUREJAVA (env, getCurrentOwnerActivity ());
}

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentAction (JNIEnv *env, jobject obj)
{
  return SD_ENSUREJAVA (env, getCurrentAction());
}


JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentActivity (JNIEnv *env, jobject obj)
{
  return SD_ENSUREJAVA (env, getCurrentActivity());
}


JNIEXPORT int JNICALL
Java_swarm_SwarmEnvironment_getCurrentTime (JNIEnv *env, jobject obj)
{
  return getCurrentTime ();
}

JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_createProbeDisplay (JNIEnv * env, jobject obj, 
						jobject anObject)
{
  return SD_ENSUREJAVA (env, CREATE_PROBE_DISPLAY (SD_FINDOBJC (env, anObject)));
}

JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_createCompleteProbeDisplay (JNIEnv * env, 
							jobject obj, 
							jobject anObject)
{
  return SD_ENSUREJAVA (env, CREATE_COMPLETE_PROBE_DISPLAY (SD_FINDOBJC (env, anObject)));
}

JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_createArchivedProbeDisplay (JNIEnv * env, 
							jobject obj, 
							jobject anObject)
{
  return SD_ENSUREJAVA (env, CREATE_ARCHIVED_PROBE_DISPLAY (SD_FINDOBJC (env, anObject)));
}


JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_createArchivedCompleteProbeDisplay (JNIEnv * env, 
							   jobject obj, 
							   jobject anObject)
{
  return 
    SD_ENSUREJAVA (env, CREATE_ARCHIVED_COMPLETE_PROBE_DISPLAY (SD_FINDOBJC (env, anObject)));
}


JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_setWindowGeometryRecordName (JNIEnv * env, 
							 jobject obj, 
							 jobject anObject)
{
  return 
    SD_ENSUREJAVA (env, SET_WINDOW_GEOMETRY_RECORD_NAME (SD_FINDOBJC (env, anObject)));
}

JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_setComponentWindowGeometryRecordNameFor (JNIEnv * env, jobject obj, jobject anObj, jobject widget)
{
  return SD_ENSUREJAVA (env, SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME_FOR (SD_FINDOBJC (env, anObj), SD_FINDOBJC (env, widget)));
}

JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_setComponentWindowGeometryRecordName (JNIEnv * env,
								  jobject obj,
								  jobject anObj)
{
  return SD_ENSUREJAVA (env, SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME_FOR (SD_FINDOBJC (env, obj), SD_FINDOBJC (env, anObj)));
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
  swarm_directory_init (env, obj);
}

