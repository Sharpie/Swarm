#include <misc/avl.h>
#include <jni.h>
#include <stdlib.h>
#include "directory.h"

avl_tree *_directory_tree;


int 
_directory_compare_jobjects (const void *A, const void *B, void *PARAM)
{
  if (((jobject_id *) A)->javaobject < ((jobjid *) B)->javaobject)
    return -1;
  return (((jobject_id *) A)->javaobject > ((jobjid *) B)->javaobject);
}

jobject_id *
_directory_findEntry (jobject javaobject)
{
   jobject_id pattern;
   jobject_id *result; 
  
   patern.javaobject = javaobject;
   result = avl_find (_directory_tree, &pattern);
   if (!result) 
     abort();

   return result;
}
   

jobject_id * 
_directory_addNewEntry (jobject javaobject, id objcobject)
{
  jobject_id * data;
  
  data = malloc (sizeof (jobject_id));
  data->objcobject = objcobject;
  data->javaobject = javaobject;

  avl_insert (_directory_tree, data);
  return data;
}


avl_tree * 
initDirectoryTree (void)
{
  _directory_tree = avl_create (_directory_compare_jobjects, NULL);
  return _directory_tree;
}

void
removeDirectoryTree (void)
{
  avl_free(_directory_tree);
}

