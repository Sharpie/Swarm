#include "directory.h"
#include <misc.h>
#include <misc/avl.h>
#include <jni.h>
#include <objc/objc.h>

static avl_tree *directory_tree;

static int 
compare_objects (const void *A, const void *B, void *PARAM)
{
  if (((jobject_id *) A)->java_object < ((jobject_id *) B)->java_object)
    return -1;

  return (((jobject_id *) A)->java_object > ((jobject_id *) B)->java_object);
}

jobject_id *
java_directory_find (jobject java_object)
{
  jobject_id pattern;
  jobject_id *result; 
  
  pattern.java_object = java_object;
  result = avl_find (directory_tree, &pattern);
  if (!result) 
    abort ();
  
  return result;
}


jobject_id * 
java_directory_add (jobject java_object, id objc_object)
{
  jobject_id *data;
  
  data = xmalloc (sizeof (jobject_id));
  data->objc_object = objc_object;
  data->java_object = java_object;
  
  avl_insert (directory_tree, data);
  return data;
}

void
java_directory_init (void)
{
  directory_tree = avl_create (compare_objects, NULL);
}

void
java_directory_drop (void)
{
  avl_free (directory_tree);
}
