// Include before jni.h to avoid jmalloc screwing up stdlib malloc decls.
#include <misc.h> 
#include <swarmconfig.h>
#ifdef JNI_H_NEEDS_INT64
#define __int64 INT64
#endif
#ifdef __osf__
#define _REENTRANT
#endif

#ifdef HAVE_JDK
#undef SIZEOF_LONG /* Kaffe can define this (it's from swarmconfig.h) */
#include <jni.h>
#include <misc/avl.h>
#include <objc/objc.h>

extern JNIEnv *jniEnv;

typedef struct jobject_id
{
  jobject java_object;
  id objc_object;
} jobject_id;

void java_directory_init (JNIEnv *jniEnv,
                          jobject swarmEnvironment);


void java_directory_drop (JNIEnv *jniEnv);

jobject_id *java_directory_java_find (JNIEnv *env, jobject java_object);
id java_directory_java_find_objc (JNIEnv *env, jobject java_object);

jobject_id *java_directory_objc_find (JNIEnv *env, id objc_object, BOOL createFlag);
jobject java_directory_objc_find_java (JNIEnv *env, id objc_object, BOOL createFlag);

jobject_id *java_directory_update (JNIEnv *env, 
                                   jobject java_object,
                                   id objc_object);
jobject_id *java_directory_switchupdate (JNIEnv *env,
                                         jobject old_java_object,
                                         jobject new_java_object,
                                         id objc_object);
jobject java_directory_update_java (JNIEnv *env,
                                    jobject java_object,
                                    id objc_object);
jobject java_directory_switchupdate_java (JNIEnv *env,
                                          jobject old_java_object,
                                          jobject new_java_object,
                                          id objc_object);

jobject java_instantiate (JNIEnv *jniEnv, jclass clazz);
jobject java_instantiate_using (JNIEnv *jniEnv, jobject jobj);

#define JFINDOBJC(env, jobj) java_directory_java_find_objc (env, jobj)
#define JFINDJAVA(env, objc) java_directory_objc_find_java (env, objc, NO)
#define JENSUREJAVA(env, objc) java_directory_objc_find_java (env, objc, YES)
#define JUPDATE(env, jobj, objc) java_directory_update_java (env, jobj, objc)
#define JSWITCHUPDATE(env, oldjobj, newjobj, objc) java_directory_switchupdate_java(env, oldjobj, newjobj, objc)
#define JINSTANTIATE(env, clazz) java_instantiate (env, clazz)
#define JINSTANTIATEUSING(env, jobj) java_instantiate_using (env, jobj)

SEL java_ensure_selector (JNIEnv *env, jobject jsel);
#define JENSUREOBJCMETHOD(env, jobj) (java_ensure_selector (env, jobj))

Class java_ensure_class (JNIEnv *env, jclass class);
#define JENSUREOBJCCLASS(env, jclass) (java_ensure_class (env, jclass))

const char *java_copy_string (JNIEnv *env, jstring javaString);
void java_cleanup_strings (JNIEnv *env, const char **stringArray, size_t count);

#define COPYSTRING(env, javaString) java_copy_string (env, javaString)
#define CLEANUPSTRINGS(env, stringArray) java_cleanup_strings (env, stringArray, sizeof (stringArray) / sizeof (const char *))
#endif
