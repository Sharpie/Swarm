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

#import <SwarmTop.h> // initSwarm
#include <swarmconfig.h> // HAVE_KAFFE
#import "../../src/defobj/java.h" 

id java_swarmEnvironmentCreating;

void
swarm_java_constructors ()
{
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
}


JNIEXPORT void JNICALL
Java_swarm_SwarmEnvironmentImpl_initSwarm (JNIEnv *env,
                                           jobject obj,
                                           jstring appName,
                                           jstring version,
                                           jstring bugAddress,
                                           jobjectArray args)
{
  JNIEXPORT void JNICALL Java_swarm_SwarmEnvironmentImpl__initSwarm___Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2_3Ljava_lang_String_2 (JNIEnv *env, jobject obj, jstring appName, jstring version, jstring bugAddress, jobjectArray args);
  jobject createPhase;
  jniEnv = env;
  createPhase = SD_JAVA_FIND_OBJECT_JAVA (java_swarmEnvironmentCreating);
  Java_swarm_SwarmEnvironmentImpl__initSwarm___Ljava_lang_String_2Ljava_lang_String_2Ljava_lang_String_2_3Ljava_lang_String_2 (env, createPhase, appName, version, bugAddress, args);
  (void) SD_JAVA_SWITCHPHASE (createPhase, java_swarmEnvironmentCreating);
}
