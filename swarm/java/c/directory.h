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
jobject_id *java_directory_find (jobject java_object);
jobject_id *java_directory_add (jobject java_object, id objc_object);

#define JFIND(jobj) (java_directory_find (jobj))->objc_object
#define JADD(jobj, objc) java_directory_add (jobj, objc);

