#include "directory.h"
#include <misc.h> // abort
#include <jni.h>

static void initialize (void) __attribute__ ((constructor));
static void shutdown (void) __attribute__ ((destructor));

static JavaVM *jvm;
static JDK1_1InitArgs vm_args;

JNIEnv *jniEnv;

static void
initialize (void)
{
  JNI_GetDefaultJavaVMInitArgs (&vm_args);
  vm_args.abort = abort;
  vm_args.exit = (void (*)(jint code))abort;

  if (JNI_CreateJavaVM (&jvm, (void **) &jniEnv, &vm_args) < 0)
    abort ();

  java_directory_init ();
}

static void
shutdown (void)
{
  (*jvm)->DestroyJavaVM (jvm);
  java_directory_drop ();
}
