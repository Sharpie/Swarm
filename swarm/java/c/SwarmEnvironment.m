// include first to avoid Kaffe jmalloc/stdlib conflict
#import <simtools.h> // initSwarm, swarmGUIMode
#import <defobj.h> // defobj_java_call_init_tables, SSTRDUP
#import "directory.h" // java_directory_init, JAVA_APPNAME
#import <simtoolsgui.h> // probe display macros

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
Java_swarm_SwarmEnvironment_createProbeDisplay (JNIEnv *env,
                                                jobject obj, 
						jobject anObject)
{
  return SD_ENSUREJAVA (env, CREATE_PROBE_DISPLAY (SD_FINDOBJC (env, anObject)));
}

JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_createCompleteProbeDisplay (JNIEnv *env, 
							jobject obj, 
							jobject anObject)
{
  return SD_ENSUREJAVA (env, CREATE_COMPLETE_PROBE_DISPLAY (SD_FINDOBJC (env, anObject)));
}

JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_createArchivedProbeDisplay (JNIEnv * env, 
							jobject obj, 
							jobject anObject,
                                                        jstring objectName)
{
  jboolean isCopy;
  jobject probeDisplay;

  const char *name = (*env)->GetStringUTFChars (env, objectName, &isCopy);
  
  probeDisplay =
    SD_ENSUREJAVA (env,
                   createArchivedProbeDisplayNamed
                   (SD_FINDOBJC (env, anObject), name));
  (*env)->ReleaseStringUTFChars (env, objectName, name);
  return probeDisplay;
}


JNIEXPORT jobject JNICALL 
Java_swarm_SwarmEnvironment_createArchivedCompleteProbeDisplay (JNIEnv * env, 
                                                                jobject obj, 
                                                                jobject anObject,
                                                                jstring objectName)
{
  jboolean isCopy;
  jobject probeDisplay;

  const char *name = (*env)->GetStringUTFChars (env, objectName, &isCopy);

  probeDisplay =
    SD_ENSUREJAVA (env,
                   createArchivedCompleteProbeDisplayNamed
                   (SD_FINDOBJC (env, anObject), name));
  
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
    SD_ENSUREJAVA (env,
                   [SD_FINDOBJC (env, anObject) setWindowGeometryRecordName: name]);
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
  id objcObject = SD_FINDOBJC (env, anObj);
  id objcWidget = SD_FINDOBJC (env, widget);
  jboolean isCopy;
  const char *name = (*env)->GetStringUTFChars (env, widgetName, &isCopy);
  jobject ret =
    SD_ENSUREJAVA (env, 
                   [objcObject setWindowGeometryRecordNameForComponent: 
                                 name widget: objcWidget]);
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
    extern void libjavaswarmstubs_constructor (void);
    extern void libjavaswarm_constructor (void);

    libjavaswarmstubs_constructor ();
    libjavaswarm_constructor ();
  }
#endif
  swarm_directory_init (env, obj);
  {
    jclass class = (*env)->GetObjectClass (env, obj);
    jfieldID fid;

    if (!(fid = (*env)->GetFieldID (env, class, "guiFlag", "Z")))
      abort ();
    
    (*env)->SetBooleanField (env, obj, fid, (jboolean) swarmGUIMode);
  }
}

