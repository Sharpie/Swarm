#include "directory.h"
#include <misc.h>
#include <misc/avl.h>
#include <jni.h>
#include <objc/objc.h>
#include <objc/objc-api.h>

#import "JavaProxy.h"
#import <defobj.h>

static avl_tree *java_tree;
static avl_tree *objc_tree;

BOOL initFlag = NO;

static jclass c_boolean,
  c_char, c_byte,
  c_int, 
  c_short, c_long,
  c_float, c_double,
  c_object, c_void,
  c_globalZone;

static void
create_class_refs (JNIEnv *env)
{
  jclass find (const char *name)
    {
      char class_name_buf[10 + strlen (name) + 1];
      char *p;
      jclass ret, clazz;
      jfieldID field;
      
      p = stpcpy (class_name_buf, "java/lang/");
      p = stpcpy (p, name);
      clazz = (*env)->FindClass (env, class_name_buf);
      if (clazz == NULL)
        abort ();
      if (!(field = (*env)->GetStaticFieldID (env,
                                                clazz,
                                                "TYPE",
                                                "Ljava/lang/Class;")))
          abort ();
      if (!(ret = (*env)->GetStaticObjectField (env, clazz, field)))
          abort ();  

      return ret;
    }
  if (!initFlag)
    {
      c_char = find ("Character");
      c_byte= find ("Byte");
      c_int = find ("Integer");
      c_short = find ("Short");
      c_long = find ("Long");
      c_float = find ("Float");
      c_double = find ("Double");
      c_void = find ("Void");
      c_object = (*env)->FindClass (env, "java/lang/Object");

      if (c_object == NULL)
        abort ();
      
      {
        jclass clazz;
        jfieldID field;

        if (!(clazz = (*env)->FindClass (env, "swarm/SwarmEnvironment")))
          abort ();
        if (!(field = (*env)->GetStaticFieldID (env,
                                                clazz,
                                                "globalZone",
                                                "Ljava/lang/Class;")))
          abort ();

        if (!(c_globalZone = (*env)->GetStaticObjectField (env, clazz, field)))
          abort ();
      }
      initFlag = YES;
    }
}

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
    result = java_directory_update (java_object,
                                    [JavaProxy create: globalZone]);
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
java_directory_update (jobject java_object, id objc_object)
{
  jobject_id *data;
  jobject_id **foundptr;
  
  data = xmalloc (sizeof (jobject_id));
  data->java_object = java_object;
  data->objc_object = objc_object;
  
  foundptr = (jobject_id **) avl_probe (java_tree, data);
  (*foundptr)->objc_object = objc_object;

  foundptr = (jobject_id **) avl_probe (objc_tree, data);
  (*foundptr)->java_object = java_object;

  if (*foundptr != data)
    XFREE (data);
    
  return *foundptr;
}

jobject_id * 
java_directory_switchupdate (jobject old_java_object,
                             jobject new_java_object,
                             id objc_object)
{
  jobject_id old;
  jobject_id *data;
  
  old.java_object = old_java_object;
  old.objc_object = objc_object;
  avl_delete (java_tree, &old);
  avl_delete (objc_tree, &old);

  data = xmalloc (sizeof (jobject_id));
  data->objc_object = objc_object;
  data->java_object = new_java_object;

  return java_directory_update (new_java_object, objc_object);
}

jobject
java_directory_update_java (jobject java_object, id objc_object)
{
  return java_directory_update (java_object, objc_object)->java_object;
}

jobject
java_directory_switchupdate_java (jobject old_java_object,
                                  jobject new_java_object,
                                  id objc_object)
{
  return java_directory_switchupdate (old_java_object,
                                      new_java_object,
                                      objc_object)->java_object;
}

jobject
java_instantiate (JNIEnv *env, jclass clazz)
{
  return (*env)->AllocObject (env, clazz);
}

jobject
java_instantiate_name (JNIEnv *env, const char *className)
{
  jclass clazz = (*env)->FindClass (env, className);
  
  if (!clazz)
    abort ();
  
  return java_instantiate (env, clazz);
}

void
java_directory_init (JNIEnv *env)
{
  java_tree = avl_create (compare_java_objects, NULL);
  objc_tree = avl_create (compare_objc_objects, NULL);
  
  create_class_refs (env);
  java_directory_update (c_globalZone, globalZone);
}

void
java_directory_drop (void)
{
  avl_destroy (java_tree, NULL);
  avl_free (objc_tree);
}

SEL
java_ensure_selector (JNIEnv *env, jobject jsel)
{
  jclass clazz = (*env)->GetObjectClass (env, jsel);
  jfieldID nameFid =
    (*env)->GetFieldID (env, clazz, "name", "Ljava/lang/String;");
  jstring string;
  const char *utf;
  char *name, *p;
  SEL sel;
  jboolean copyFlag;

  string = (*env)->GetObjectField (env, jsel, nameFid);
  utf = (*env)->GetStringUTFChars (env, string, &copyFlag);
  name = copyFlag ? (char *) utf : strdup (utf);
  
  for (p = name; *p; p++)
    if (*p == '$')
      *p = ':';

  sel = sel_get_any_typed_uid (name);

  if (!sel)
    {
      jfieldID retTypeFid = 
        (*env)->GetFieldID (env, clazz, "retType", "Ljava/lang/Class;");
      jfieldID argTypesFid =
        (*env)->GetFieldID (env, clazz, "types", "[Ljava/lang/Class;");
      jclass retType = (*env)->GetObjectField (env, jsel, retTypeFid);
      jarray argTypes = (*env)->GetObjectField (env, jsel, argTypesFid);
      jsize tc = (*env)->GetArrayLength (env, argTypes);
      jsize ti;
      char signatureBuf[tc * 6], *p = signatureBuf;
      size_t pos = 0;

      size_t alignto (size_t pos, size_t alignment)
        {
          size_t mask = (alignment - 1);
          
          if ((pos & mask) == 0)
            return pos;
          else
            return (pos + alignment) & ~mask;
        }

      void add (jobject class, BOOL regFlag)
        {
          char type;

          if (class == c_object)
            {
              type = _C_ID;
              pos = alignto (pos, __alignof__ (id));
            }
          else if (class == c_int)
            {
              type = _C_INT;
              pos = alignto (pos, __alignof__ (int));
            }
          else if (class == c_short)
            {
              type = _C_SHT;
              pos = alignto (pos, __alignof__ (short));
            }
          else if (class == c_long)
            {
              type = _C_LNG;
              pos = alignto (pos, __alignof__ (long));
            }
          else if (class == c_boolean)
            {
              type = _C_UCHR;
              pos = alignto (pos, __alignof__ (BOOL));
            }
          else if (class == c_char)
            {
              type = _C_CHR;
              pos = alignto (pos, __alignof__ (char));
            }
          else if (class == c_float)
            {
              type = _C_FLT;
              pos = alignto (pos, __alignof__ (float));
            }
          else if (class == c_double)
            {
              type = _C_DBL;
              pos = alignto (pos, __alignof__ (double));
            }
          else
            abort ();
          *p++ = type;
          if (regFlag)
            *p++ = '+';
          {
            char posbuf[4];
            
            sprintf (posbuf, "%u", pos);
            p = stpcpy (p, posbuf);
          }
        }
      
      add (retType, NO);
      for (ti = 0; ti < tc; ti++)
        add ((*env)->GetObjectArrayElement (env, argTypes, ti), YES);

      printf ("[%s][%s]\n", name, signatureBuf);
      sel = sel_register_typed_name (name, signatureBuf);
    }
  
  if (copyFlag)
    (*env)->ReleaseStringUTFChars (env, string, utf);
  else
    XFREE (name);
  return sel;
}
