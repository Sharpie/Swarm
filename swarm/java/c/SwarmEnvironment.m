#include "SwarmEnvironment.h"
#include <simtools.h> // initSwarm
#include <defobj.h> // defobj_java_call_init_tables
#include "directory.h" // java_directory_init
#include <misc.h> // xmalloc, strdup

void 
Java_swarm_SwarmEnvironment_initSwarm (JNIEnv *env, jobject obj, jobjectArray args)
{
  int i = 0;
  const char **argv;
  int argc;
  const char *utf;
  jstring jstr;
  jboolean isCopy;

  argc = (*env)->GetArrayLength (env, args) + 1;   
  argv = (const char **) xmalloc (sizeof (const char *) * argc);

  argv[0] = "java";
  for (i = 0; i < argc - 1; i++)
    {
      jstr = (*env)->GetObjectArrayElement (env, args, i);
      utf = (const char *)(*env)->GetStringUTFChars (env, jstr, &isCopy);
      argv[i + 1] = isCopy ? (const char *) utf : strdup (utf);
    }
  java_directory_init (env);
  defobj_init_java_call_tables ((void *) env);
  initSwarm (argc, (const char **)argv);
}

