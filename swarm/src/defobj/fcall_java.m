// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <swarmconfig.h>
#if defned(HAVE_JDK) && defined(USE_AVCALL)

#ifdef __CYGWIN__
#ifndef HAVE_KAFFE
#define MSVC
#endif

#import "fcall_java.h"
#include <avcall.h>
#import "java.h"
#import <defobj/FArguments.h>

void
java_setup_call (FArguments_c *fa,
                 jobject obj,
                 jmethodID method)
{
  av_ptr (fa->java_avalist, JNIEnv *, jniEnv);
  av_ptr (fa->java_avalist, jobject, obj);
  av_ptr (fa->java_avalist, jmethodID, method);
}

void
java_setup_static_call (FArguments_c *fa,
                        jclass class,
                        jmethodID method)
{
  av_ptr (fa->java_avalist, JNIEnv *, jniEnv);
  av_ptr (fa->java_avalist, jclass, class);
  av_ptr (fa->java_avalist, jmethodID, method);
}

#define JAVA
#include "_fcall.m"
#endif
#endif
