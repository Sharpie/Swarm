#include <misc/avl.h>
#include <jni.h>

typedef struct jobject_id
{
  jobject javaobject;
  id objcobject;
} jobject_id;

avl_tree *initDirectoryTree (void);

void removeDirectoryTree (void);

#define DIRECTORY_ID_LOOKUP(jobj) \
((jobject_id *) _directory_findEntry (jobj))->objcobject

#define DIRECTORY_ADD_NEW_ENTRY(jobj, objc) \
_directory_addNewEntry (jobj, objc);
