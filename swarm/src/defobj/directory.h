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
Class swarm_directory_get_swarm_class (id object);

#ifdef HAVE_JDK
#undef SIZEOF_LONG /* Kaffe can define this (it's from swarmconfig.h) */
#include <jni.h>
#include <misc/avl.h>

#import <defobj/Create.h>

extern JNIEnv *jniEnv;

void swarm_directory_init (JNIEnv *jniEnv,
                          jobject swarmEnvironment);


jobject swarm_directory_java_instantiate (JNIEnv *jniEnv, jclass clazz);
jobject swarm_directory_next_phase (JNIEnv *jniEnv, jobject jobj);
unsigned swarm_directory_java_hash_code (jobject javaObject);

@interface DirectoryEntry: CreateDrop
{
@public
  jobject javaObject;
  id object;
}
- setJavaObject: (jobject)javaObject;
- setObject: object;
- (int)compare: obj;
- (const char *)getObjcName;
- (unsigned)getHashCode;
- (void)drop;
- (void)describe: outputCharStream;
@end

@interface Directory: CreateDrop
{
  id *table;
  avl_tree *objc_tree;
  DirectoryEntry *findEntry;
}
+ createBegin: aZone;
- add: object javaObject: (jobject)javaObject;
- javaFind: (jobject)javaObject;
- javaFindObjc: (jobject)javaObject;
- objcFind: object;
- (jobject)objcFindJava: object;
- (jclass)objcFindJavaClass: (Class)class;
- switchJava: object javaObject: (jobject)javaObject;
- nextPhase: object currentJavaPhase: (jobject)javaObject;
- switchObjc: object javaObject: (jobject)javaObject;
- javaEnsureObjc: (jobject)javaObject;
- (jobject)objcEnsureJava: object;
- (BOOL)objcRemove: obj;
- (void)describe: outputCharStream;
@end

extern id swarmDirectory;

#define SD_FINDOBJC(env, jobj) [swarmDirectory javaFindObjc: jobj]
#define SD_ENSUREOBJC(env, jobj) [swarmDirectory javaEnsureObjc: jobj]
#define SD_FINDJAVA(env, objc) [swarmDirectory objcFindJava: objc]
#define SD_ENSUREJAVA(env, objc) [swarmDirectory objcEnsureJava: objc]
#define SD_FINDJAVACLASS(env, objcClass) [swarmDirectory objcFindJavaClass: objcClass]
#define SD_ADD(env, jobj, objc) [swarmDirectory add: objc javaObject: jobj]
#define SD_ADDJAVA(env, jobj, objc) (((DirectoryEntry *) SD_ADD (env, jobj, objc))->javaObject)
#define SD_SWITCHJAVA(env, newjobj, objc) (((DirectoryEntry *) [swarmDirectory switchJava: objc javaObject: newjobj])->javaObject)
#define SD_NEXTPHASE(env, jobj, objc) (((DirectoryEntry *) [swarmDirectory nextPhase: objc currentJavaPhase: jobj])->javaObject)
#define SD_SWITCHOBJC(env, jobj, newobjc) [swarmDirectory switchObjc: newobjc javaObject: jobj]
#define SD_INSTANTIATE(env, clazz) swarm_directory_java_instantiate (env, clazz)
#define SD_NEXTJAVAPHASE(env, jobj) swarm_directory_next_phase (env, jobj)

SEL swarm_directory_ensure_selector (JNIEnv *env, jobject jsel);
#define SD_ENSUREOBJCMETHOD(env, jobj) (swarm_directory_ensure_selector (env, jobj))

Class swarm_directory_ensure_class (JNIEnv *env, jclass javaClass);
#define SD_ENSUREOBJCCLASS(env, jclass) (swarm_directory_ensure_class (env, jclass))

const char *swarm_directory_copy_java_string (JNIEnv *env, jstring javaString);
void swarm_directory_cleanup_strings (JNIEnv *env, const char **stringArray, size_t count);

const char *swarm_directory_get_language_independent_class_name (id object);

#define SD_COPYSTRING(env, javaString) swarm_directory_copy_java_string (env, javaString)
#define SD_CLEANUPSTRINGS(env, stringArray) swarm_directory_cleanup_strings (env, stringArray, sizeof (stringArray) / sizeof (const char *))
#endif

