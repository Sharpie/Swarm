// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools.h> // initSwarm, swarmGUIMode
#import <defobj.h> // defobj_java_call_init_tables, SSTRDUP
#import <simtoolsgui.h> // probe display macros
#include <swarmconfig.h> // HAVE_KAFFE
#import "../../src/defobj/java.h" // SD_JAVA_ENSURE_JAVA, swarm_directory_dump, swarm_directory_java_associate_objects
#import <defobj/directory.h> // swarmDirectory

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentSwarm (JNIEnv *env, jobject obj)
{
  return SD_JAVA_ENSUREJAVA (getCurrentSwarm ());
}

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentSchedule (JNIEnv *env, jobject obj)
{
  return SD_JAVA_ENSUREJAVA (getCurrentSchedule ());
}

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentSwarmActivity (JNIEnv *env, jobject obj)
{
  return SD_JAVA_ENSUREJAVA (getCurrentSwarmActivity ());
}

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentScheduleActivity (JNIEnv *env, 
							jobject obj)
{
  return SD_JAVA_ENSUREJAVA (getCurrentScheduleActivity ());
}


JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentOwnerActivity (JNIEnv *env, jobject obj)
{
  return SD_JAVA_ENSUREJAVA (getCurrentOwnerActivity ());
}

JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentAction (JNIEnv *env, jobject obj)
{
  return SD_JAVA_ENSUREJAVA (getCurrentAction ());
}


JNIEXPORT jobject JNICALL
Java_swarm_SwarmEnvironment_getCurrentActivity (JNIEnv *env, jobject obj)
{
  return SD_JAVA_ENSUREJAVA (getCurrentActivity ());
}


JNIEXPORT int JNICALL
Java_swarm_SwarmEnvironment_getCurrentTime (JNIEnv *env, jobject obj)
{
  return getCurrentTime ();
}

JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_createProbeDisplay (JNIEnv *env,
                                                jobject obj, 
						jobject anObject)
{
  return SD_JAVA_ENSUREJAVA (CREATE_PROBE_DISPLAY (SD_JAVA_ENSUREOBJC (anObject)));
}

JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_createCompleteProbeDisplay (JNIEnv *env, 
							jobject obj, 
							jobject anObject)
{
  return SD_JAVA_ENSUREJAVA (CREATE_COMPLETE_PROBE_DISPLAY (SD_JAVA_ENSUREOBJC (anObject)));
}

JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_createArchivedProbeDisplay (JNIEnv *env, 
							jobject obj, 
							jobject anObject,
                                                        jstring objectName)
{
  jboolean isCopy;
  jobject probeDisplay;

  const char *name = (*env)->GetStringUTFChars (env, objectName, &isCopy);
  
  probeDisplay =
    SD_JAVA_ENSUREJAVA (createArchivedProbeDisplayNamed
                        (SD_JAVA_ENSUREOBJC (anObject), name));
  (*env)->ReleaseStringUTFChars (env, objectName, name);
  return probeDisplay;
}


JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_createArchivedCompleteProbeDisplay (JNIEnv *env, 
                                                                jobject obj, 
                                                                jobject anObject,
                                                                jstring objectName)
{
  jboolean isCopy;
  jobject probeDisplay;

  const char *name = (*env)->GetStringUTFChars (env, objectName, &isCopy);

  probeDisplay =
    SD_JAVA_ENSUREJAVA (createArchivedCompleteProbeDisplayNamed
                        (SD_JAVA_ENSUREOBJC (anObject), name));
  
  (*env)->ReleaseStringUTFChars (env, objectName, name);
  return probeDisplay;
}


JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_setWindowGeometryRecordName (JNIEnv *env, 
							 jobject obj, 
							 jobject anObject,
                                                         jstring objectName)
{
  jboolean isCopy;
  const char *name = (*env)->GetStringUTFChars (env, objectName, &isCopy);
  jobject ret;

  ret =
    SD_JAVA_ENSUREJAVA ([SD_JAVA_FINDOBJC (anObject) setWindowGeometryRecordName: name]);
  if (isCopy)
    (*env)->ReleaseStringUTFChars (env, objectName, name);
  return ret;
}

JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_setComponentWindowGeometryRecordNameFor (JNIEnv *env,
                                                                     jobject obj,
                                                                     jobject anObj,
                                                                     jobject widget,
                                                                     jobject widgetName)
{
  id objcObject = SD_JAVA_FINDOBJC (anObj);
  id objcWidget = SD_JAVA_FINDOBJC (widget);
  jboolean isCopy;
  const char *name = (*env)->GetStringUTFChars (env, widgetName, &isCopy);
  jobject ret =
    SD_JAVA_ENSUREJAVA ([objcObject setWindowGeometryRecordNameForComponent: name
                                    widget: objcWidget]);
  if (isCopy)
    (*env)->ReleaseStringUTFChars (env, widgetName, name);
  return ret;
}

JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_setComponentWindowGeometryRecordName (JNIEnv *env,
								  jobject obj,
								  jobject widget,
                                                                  jobject widgetName)
{ 
  return Java_swarm_SwarmEnvironment_setComponentWindowGeometryRecordNameFor (env, obj, obj, widget, widgetName);
}

JNIEXPORT void JNICALL
Java_swarm_SwarmEnvironment_initSwarm (JNIEnv *env,
                                       jobject obj,
                                       jstring appName,
                                       jstring version,
                                       jstring bugAddress,
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

  argv[0] = (*env)->GetStringUTFChars (env, appName, &isCopy);
  for (i = 0; i < argc - 1; i++)
    {
      jstr = (*env)->GetObjectArrayElement (env, args, i);
      utf = (const char *) (*env)->GetStringUTFChars (env, jstr, &isCopy);
      argv[i + 1] = isCopy ? (const char *) utf : SSTRDUP (utf);
    }

  defobj_init_java_call_tables ((void *) env);
  _initSwarm_ (argc, argv,
               argv[0],
               (*env)->GetStringUTFChars (env, version, &isCopy),
               (*env)->GetStringUTFChars (env, bugAddress, &isCopy),
               NULL, NULL, NULL, NO, YES);
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
  jniEnv = env;
  swarmDirectory = [Directory create: globalZone];
  swarm_directory_java_associate_objects (obj);
  {
    jclass class = (*env)->GetObjectClass (env, obj);
    jfieldID fid;

    if (!(fid = (*env)->GetFieldID (env, class, "guiFlag", "Z")))
      abort ();
    
    (*env)->SetBooleanField (env, obj, fid, (jboolean) swarmGUIMode);
  }
}


JNIEXPORT void JNICALL
Java_swarm_SwarmEnvironment_xprint (JNIEnv *env, jobject swarmEnv, jobject obj)
{
  xprint (SD_JAVA_FINDOBJC (obj));
}

JNIEXPORT void JNICALL
Java_swarm_SwarmEnvironment_xfprint (JNIEnv *env, jobject swarmEnv, jobject obj)
{
  xfprint (SD_JAVA_FINDOBJC (obj));
}

JNIEXPORT void JNICALL
Java_swarm_SwarmEnvironment_dumpDirectory (JNIEnv *env, jobject swarmEnv)
{
  swarm_directory_dump ();
}
