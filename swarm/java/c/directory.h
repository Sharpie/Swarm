#include <misc/avl.h>
#include <jni.h>
#include <objc/objc.h>

typedef struct jobject_id
{
  jobject java_object;
  id objc_object;
} jobject_id;

void java_directory_init (void);
void java_directory_drop (void);
jobject_id *java_directory_java_find (jobject java_object);
jobject_id *java_directory_objc_find (id objc_object);
jobject_id *java_directory_add (jobject java_object, id objc_object);

#define JFINDOBJC(jobj) ((java_directory_java_find (jobj))->objc_object)
#define JFINDJAVA(obj) ((java_directory_objc_find (obj))->java_object)
#define JADD(jobj, objc) java_directory_add (jobj, objc);

SEL java_ensure_selector (JNIEnv *env, jobject jsel);
#define JFINDOBJCMETHOD(env, jobj) (java_ensure_selector (env, jobj))

