#ifdef __CYGWIN__
#define MSVC
#endif

#import "fcall_java.h"
#include <avcall.h>
#import <defobj/directory.h> // JNI things
#import <defobj/FArguments.h>

#ifdef HAVE_JDK
void
java_setup_call (FArguments_c *fa,
                 JNIEnv *env,
                 jobject obj,
                 jmethodID method)
{
  av_ptr (fa->java_avalist, JNIEnv *, env);
  av_ptr (fa->java_avalist, jobject, obj);
  av_ptr (fa->java_avalist, jmethodID, method);
}

void
java_setup_static_call (FArguments_c *fa,
                        JNIEnv *env,
                        jclass class,
                        jmethodID method)
{
  av_ptr (fa->java_avalist, JNIEnv *, env);
  av_ptr (fa->java_avalist, jclass, class);
  av_ptr (fa->java_avalist, jmethodID, method);
}
#endif

#define JAVA
#include "_fcall.m"
