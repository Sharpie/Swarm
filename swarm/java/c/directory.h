#include <misc/avl.h>
#include <jni.h>
#include <objc/objc.h>

typedef struct jobject_id
{
  jobject java_object;
  id objc_object;
} jobject_id;

void java_directory_init (JNIEnv *jniEnv, jobject swarmEnvironment);
void java_directory_drop (JNIEnv *jniEnv);

jobject_id *java_directory_java_find (JNIEnv *env, jobject java_object);
jobject_id *java_directory_objc_find (id objc_object);

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
jobject java_instantiate_using_from_class (JNIEnv *jniEnv, jclass jobj);
jobject java_instantiate_using_from_object (JNIEnv *jniEnv, jobject jobj);

#define JFINDOBJC(env, jobj) ((java_directory_java_find (env, jobj))->objc_object)
#define JFINDJAVA(obj) ((java_directory_objc_find (obj))->java_object)
#define JUPDATE(env, jobj, objc) java_directory_update_java (env, jobj, objc)
#define JSWITCHUPDATE(env, oldjobj, newjobj, objc) java_directory_switchupdate_java(env, oldjobj, newjobj, objc)
#define JINSTANTIATE(env, clazz) java_instantiate (env, clazz)
#define JINSTANTIATEUSINGFROMCLASS(env, jclass) java_instantiate_using_from_class (env, jclass)
#define JINSTANTIATEUSINGFROMOBJECT(env, jobj) java_instantiate_using_from_object (env, jobj)

SEL java_ensure_selector (JNIEnv *env, jobject jsel);
#define JFINDOBJCMETHOD(env, jobj) (java_ensure_selector (env, jobj))

