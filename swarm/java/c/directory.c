#include "directory.h"
#include <misc.h>
#include <misc/avl.h>
#include <jni.h>
#include <objc/objc.h>

static avl_tree *java_tree;
static avl_tree *objc_tree;

static int 
compare_java_objects (const void *A, const void *B, void *PARAM)
{
  if (((jobject_id *) A)->java_object < ((jobject_id *) B)->java_object)
    return -1;

  return (((jobject_id *) A)->java_object > ((jobject_id *) B)->java_object);
}

static int 
compare_objc_objects (const void *A, const void *B, void *PARAM)
{
  if (((jobject_id *) A)->objc_object < ((jobject_id *) B)->objc_object)
    return -1;

  return (((jobject_id *) A)->objc_object > ((jobject_id *) B)->objc_object);
}

jobject_id *
java_directory_java_find (jobject java_object)
{
  jobject_id pattern;
  jobject_id *result; 
  
  pattern.java_object = java_object;
  result = avl_find (java_tree, &pattern);
  if (!result) 
    abort ();
  
  return result;
}

jobject_id *
java_directory_objc_find (id objc_object)
{
  jobject_id pattern;
  jobject_id *result; 
  
  pattern.objc_object = objc_object;
  result = avl_find (objc_tree, &pattern);
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
  
  avl_insert (java_tree, data);
  avl_insert (objc_tree, data);
  return data;
}

void
java_directory_init (void)
{
  java_tree = avl_create (compare_java_objects, NULL);
  objc_tree = avl_create (compare_objc_objects, NULL);
}

void
java_directory_drop (void)
{
  avl_destory (java_tree, NULL);
  avl_free (objc_tree);
}
