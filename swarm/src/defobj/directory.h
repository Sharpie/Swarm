// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Include before jni.h to avoid jmalloc screwing up stdlib malloc decls.
#include <misc.h> 
#include <swarmconfig.h>
#ifdef JNI_H_NEEDS_INT64
#define __int64 INT64
#endif
#ifdef __osf__
#define _REENTRANT
#endif

#include <objc/objc.h>
Class swarm_directory_swarm_class (id object);
const char *swarm_directory_language_independent_class_name (id object);
Class swarm_directory_ensure_class_named (const char *className);

#ifdef HAVE_JDK
#undef SIZEOF_LONG /* Kaffe can define this (it's from swarmconfig.h) */
#include <jni.h>
#include <misc/avl.h>

#import <defobj/Create.h>

extern JNIEnv *jniEnv;

const char *swarm_directory_java_class_name (JNIEnv *env, jobject obj);

void swarm_directory_init (JNIEnv *jniEnv,
                          jobject swarmEnvironment);


jobject swarm_directory_java_instantiate (JNIEnv *jniEnv, jclass clazz);
jobject swarm_directory_next_phase (JNIEnv *jniEnv, jobject jobj);
unsigned swarm_directory_java_hash_code (JNIEnv *env, jobject javaObject);

@interface DirectoryEntry: CreateDrop
{
@public
  jobject javaObject;
  id object;
}
- setJavaObject: (jobject)javaObject;
- setObject: object;
void swarm_directory_entry_drop (JNIEnv *env, DirectoryEntry *entry);
void swarm_directory_entry_describe (JNIEnv *env,
                                     DirectoryEntry *entry,
                                     id outputCharStream);
- (void)describe: outputCharStream;
@end

@interface Directory: CreateDrop
{
  id *table;
  avl_tree *objc_tree;
  DirectoryEntry *findEntry;
}
+ createBegin: aZone;
id swarm_directory_java_find_objc (JNIEnv *env, jobject javaObject);
jobject swarm_directory_objc_find_java (JNIEnv *env, id object);
DirectoryEntry *swarm_directory_add (JNIEnv *env, id object, jobject lref);
DirectoryEntry *swarm_directory_switch_phase (JNIEnv *env, id nextPhase, jobject currentJavaPhase);
DirectoryEntry *swarm_directory_switch_objc (JNIEnv *env, id object, jobject javaObject);
id swarm_directory_java_ensure_objc (JNIEnv *env, jobject javaObject);
jobject swarm_directory_objc_ensure_java (JNIEnv *env, id object);
BOOL swarm_directory_objc_remove (JNIEnv *env, id obj);
jclass swarm_directory_find_java_class (JNIEnv *env, const char *javaClassName, BOOL failFlag);
jclass swarm_directory_objc_find_java_class (JNIEnv *env, Class class);

- (void)describe: outputCharStream;
@end

extern id swarmDirectory;

extern void swarm_directory_dump ();

#define SD_JAVA_FINDOBJC(env, jobj)  swarm_directory_java_find_objc (env, jobj)
#define SD_JAVA_ENSUREOBJC(env, jobj) swarm_directory_java_ensure_objc (env, jobj)
#define SD_JAVA_FINDJAVA(env, objc) swarm_directory_objc_find_java (env, objc)
#define SD_JAVA_ENSUREJAVA(env, objc) swarm_directory_objc_ensure_java (env, objc)
#define SD_JAVA_FINDJAVACLASS(env, objcClass) swarm_directory_objc_find_java_class (env, objcClass)
#define SD_JAVA_ADD(env, jobj, objc) swarm_directory_add (env, objc, jobj)
#define SD_JAVA_ADDJAVA(env, jobj, objc) swarm_directory_add (env, objc, jobj)->javaObject
#define SD_JAVA_NEXTPHASE(env, jobj, objc) swarm_directory_switch_phase (env, objc, jobj)->javaObject
#define SD_JAVA_SWITCHOBJC(env, jobj, newobjc) swarm_directory_switch_objc (env, newobjc, jobj)
#define SD_JAVA_INSTANTIATE(env, clazz) swarm_directory_java_instantiate (env, clazz)
#define SD_JAVA_NEXTJAVAPHASE(env, jobj) swarm_directory_next_phase (env, jobj)

SEL swarm_directory_ensure_selector (JNIEnv *env, jobject jsel);
#define SD_JAVA_ENSUREOBJCMETHOD(env, jobj) (swarm_directory_ensure_selector (env, jobj))

Class swarm_directory_java_ensure_class (JNIEnv *env, jclass javaClass);

#define SD_JAVA_ENSUREOBJCCLASS(env, jclass) (swarm_directory_java_ensure_class (env, jclass))

const char *swarm_directory_copy_java_string (JNIEnv *env, jstring javaString);
void swarm_directory_cleanup_strings (JNIEnv *env, const char **stringArray, size_t count);


#define SD_JAVA_COPYSTRING(env, javaString) swarm_directory_copy_java_string (env, javaString)
#define SD_JAVA_CLEANUPSTRINGS(env, stringArray) swarm_directory_cleanup_strings (env, stringArray, sizeof (stringArray) / sizeof (const char *))
#endif

#define SD_GETCLASS(obj) swarm_directory_swarm_class (obj)
