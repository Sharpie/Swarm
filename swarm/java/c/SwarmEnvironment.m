#include "SwarmEnvironment.h"
#include <directory.h>
#include <stdio.h>
#include <misc.h>
#include <simtools.h>

void 
Java_SwarmEnvironment_initSwarm(JNIEnv *env, jobject obj, jobjectArray args)
{

    jsize len; 
    int i = 0;
    char ** argv;
    int argc;
    jstring jstr;
    jboolean isCopy = 1;

    argc = (*env)->GetArrayLength (env, args);    
    argv = (char **) malloc (sizeof (char *) * len);

    for (i=0; i<argc; i++)
      {
	jstr = (*env)->GetObjectArrayElement (env, args, i);
	argv[i] = (char *)(*env)->GetStringUTFChars (env, jstr, &isCopy);
      }
    
    initSwarm (argc, (const char **)argv);
}

