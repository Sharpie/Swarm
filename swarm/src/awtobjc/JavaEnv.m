// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <javaobjc/JavaEnv.h>

#include <stdlib.h>

extern char * getenv(const char *);
extern char * strcat(char *, const char *);

id javaEnv = 0;

// XXX define "PC" in makefile.conf
#ifdef PC
#define SEPSTR ";"
#else
#define SEPSTR	":"
#endif

@implementation JavaEnv

+ new
{
  self = [JavaEnv alloc];
  fprintf (stderr, "creating... java env\n");
  [self init];
  return self;
}


- init
{
  jint res;
  char classpath[4096];
  char * env_var;
  
  JNI_GetDefaultJavaVMInitArgs(&vm_args);
  // vm_args.verbose = 1;
  
  // our classes are found in $(SWARMHOME)/lib
  sprintf(classpath, ".%s", SEPSTR);
  if ((env_var = getenv("SWARMHOME")) != NULL)
    {
      strcat(classpath, env_var);
      strcat(classpath, "/lib");
      strcat(classpath, SEPSTR);
    }
  // if we have a classpath environment, tack it on
  if ((env_var = getenv("CLASSPATH")) != NULL)
    {
      strcat(classpath, env_var);
      strcat(classpath, SEPSTR);
    }
  if (vm_args.classpath)
    // add the rest of the classpath
    strcat(classpath, vm_args.classpath);
  vm_args.classpath = classpath;
#if 1
  fprintf(stderr, "initializing; classpath=%s\n", vm_args.classpath);
  fprintf(stderr, "initializing; version=%lx\n", vm_args.version);
#endif
  vm_args.abort = abort;
  vm_args.exit = (void (*)(jint code))abort;
  res = JNI_CreateJavaVM (&jvm, &jniEnv, &vm_args);
  if (res < 0)
    {
      fprintf (stderr, "Can't create Java VM\n");
      exit (1);
    }
  fprintf (stderr, "env: %p\n", jniEnv);
  return self;
}

- (void)free
{
  (*jvm)->DestroyJavaVM (jvm);
  [super free];
}

- (JNIEnv *)getJNIEnv
{
  return jniEnv;
}

- (jclass)findClass: (const char *)className
{
  jclass cls;
  
  cls = (*jniEnv)->FindClass (jniEnv, className);
  printf ("%s %p\n", className, cls);
  if (cls == 0)
    {
      (*jniEnv)->ExceptionDescribe (jniEnv);
      fprintf (stderr, "Can't find class %s\n", className);
      exit (1);
    }
  return cls;
}

@end

