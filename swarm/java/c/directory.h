#include <misc/avl.h>
#include <jni.h>
#include <objc/objc.h>

typedef struct jobject_id
{
  jobject java_object;
  id objc_object;
} jobject_id;

void java_directory_init (JNIEnv *jniEnv);
void java_directory_drop (void);
jobject_id *java_directory_java_find (jobject java_object);
jobject_id *java_directory_objc_find (id objc_object);
jobject_id *java_directory_update (jobject java_object, id objc_object);
jobject_id *java_directory_switchupdate (jobject old_java_object, jobject new_java_object, id objc_object);
jobject java_directory_update_java (jobject java_object, id objc_object);
jobject java_directory_switchupdate_java (jobject old_java_object, jobject new_java_object, id objc_object);
jobject java_instantiate (JNIEnv *jniEnv, jclass clazz);
jobject java_instantiate_name (JNIEnv *jniEnv, const char *className);

#define JFINDOBJC(jobj) ((java_directory_java_find (jobj))->objc_object)
#define JFINDJAVA(obj) ((java_directory_objc_find (obj))->java_object)
#define JUPDATE(jobj, objc) java_directory_update_java (jobj, objc)
#define JSWITCHUPDATE(oldjobj, newjobj, objc) java_directory_switchupdate_java(oldjobj, newjobj, objc)
#define JINSTANTIATE(env, clazz) java_instantiate (env, clazz)
#define JINSTANTIATENAME(env, className) java_instantiate_name (env, className)

SEL java_ensure_selector (JNIEnv *env, jobject jsel);
#define JFINDOBJCMETHOD(env, jobj) (java_ensure_selector (env, jobj))

