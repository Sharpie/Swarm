// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#include <swarmconfig.h>
#if defined(HAVE_JDK) && defined(USE_AVCALL)

#if defined(__CYGWIN__) && !defined(HAVE_KAFFE)
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
