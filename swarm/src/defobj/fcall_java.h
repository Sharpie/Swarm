// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/FArguments.h>
#import <defobj/FCall.h>
#import <defobj/directory.h> // JNI things

#ifdef HAVE_JDK
extern void java_setup_call (FArguments_c *fa,
                             JNIEnv *env,
                             jobject obj,
                             jmethodID method);
extern void java_setup_static_call (FArguments_c *fa,
                                    JNIEnv *env,
                                    jclass class,
                                    jmethodID method);
extern void java_add_primitive (FArguments_c *fa, fcall_type_t type, void *val);
extern void java_set_return_type (FCall_c *fc, FArguments_c *fa);
extern void java_call (FArguments_c *fa);
#endif
