#include "SwarmEnvironment.h"
#include <defobj/FCall.h>
#include "directory.h"

#include <misc.h>
#include <simtools.h>

void 
Java_SwarmEnvironment_initSwarm (JNIEnv *env, jobject obj, jobjectArray args)
{
  int i = 0;
  const char **argv;
  int argc;
  const char *utf;
  jstring jstr;
  jboolean isCopy;

  argc = (*env)->GetArrayLength (env, args);    
  argv = (const char **) xmalloc (sizeof (char *) * argc);

  for (i = 0; i < argc; i++)
    {
      jstr = (*env)->GetObjectArrayElement (env, args, i);
      utf = (char *)(*env)->GetStringUTFChars (env, jstr, &isCopy);
      argv[i] = isCopy ? (char *) utf : strdup (utf);
    }
  java_directory_init (env);
  init_javacall_tables ((void *) env);
  initSwarm (argc, (const char **)argv);
}

