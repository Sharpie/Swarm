#include "directory.h"
#include <misc.h>
#include <misc/avl.h>
#include <objc/objc.h>
#include <objc/objc-api.h>

#import "JavaProxy.h"
#import <defobj.h>

#import <defobj/Program.h> // Type_c

#ifdef HAVE_JDK
static avl_tree *java_tree;
static avl_tree *objc_tree;

BOOL initFlag = NO;

static jclass c_boolean,
  c_char, c_byte,
  c_int, 
  c_short, c_long,
  c_float, c_double,
  c_object, c_string, 
  c_void;

extern JNIEnv *jniEnv;

static void
create_class_refs (JNIEnv *env)
{
  jclass find (const char *name)
    {
      char class_name_buf[10 + strlen (name) + 1];
      char *p;
      jclass ret, clazz;
      
      p = stpcpy (class_name_buf, "java/lang/");
      p = stpcpy (p, name);
      if (!(clazz = (*env)->FindClass (env, class_name_buf)))
        abort ();

      ret = (*env)->NewGlobalRef (env, clazz);
      return ret;
    }
  jclass find_primitive (const char *name)
    {
      jfieldID field;
      jclass clazz = find (name);
      jclass ret;
      
      if (!(field = (*env)->GetStaticFieldID (env,
                                              clazz,
                                              "TYPE",
                                              "Ljava/lang/Class;")))
        abort ();
      if (!(ret = (*env)->GetStaticObjectField (env, clazz, field)))
        abort ();
      ret = (*env)->NewGlobalRef (env, ret);
      return ret;
    }
  if (!initFlag)
    {
      c_char = find_primitive ("Character");
      c_byte= find_primitive ("Byte");
      c_int = find_primitive ("Integer");
      c_short = find_primitive ("Short");
      c_long = find_primitive ("Long");
      c_float = find_primitive ("Float");
      c_double = find_primitive ("Double");
      c_void = find_primitive ("Void");
      c_string = find ("String");
      c_object = find ("Object");
      
      if (c_object == NULL)
        abort ();
      
      c_object = (*env)->NewGlobalRef (env, c_object);
      
      initFlag = YES;
    }
}

static jclass
java_class_for_typename (JNIEnv *env, const char *typeName, BOOL usingFlag)
{
  extern const char *swarm_lookup_module (const char *name);
  const char *module = swarm_lookup_module (typeName);
  char javaClassName[5 + 1 + strlen (module) + 1 + strlen (typeName) + 5 + 1];
  char *p;

  p = stpcpy (javaClassName, "swarm.");
  p = stpcpy (p, module);
  p = stpcpy (p, ".");
  p = stpcpy (p, typeName);
  if (usingFlag)
    p = stpcpy (p, "U");
  p = stpcpy (p, "Impl");

  return (*env)->FindClass (env, javaClassName);
}

jobject_id *
java_directory_java_find (JNIEnv *env, jobject java_object)
{
  jobject_id pattern;
  jobject_id *result; 
  jobject newRef;

  if (!java_object)
    return NULL;

  newRef = (*env)->NewGlobalRef (env, java_object);
  
  pattern.java_object = newRef;
  result = avl_find (java_tree, &pattern);
  (*env)->DeleteGlobalRef(env, newRef);
  if ((*env)->IsInstanceOf (env, java_object, c_string))
    {
      jboolean isCopy;
      const char *utf, *str;
      
      if (result)
        XFREE (result->objc_object);

      utf = (*env)->GetStringUTFChars (env, java_object, &isCopy);
      str = strdup (utf);
      if (isCopy)
        (*env)->ReleaseStringUTFChars (env, java_object, utf);
      result = java_directory_update (env, java_object, (id) str);
    }
  else if (!result) 
    result = java_directory_update (env,
                                    java_object,
                                    [JavaProxy create: globalZone]);
  
  return result;
}

id
java_directory_java_find_objc (JNIEnv *env, jobject java_object)
{
  return (java_object
          ? java_directory_java_find (env, java_object)->objc_object
          : nil);
}

jobject_id *
java_directory_objc_find (JNIEnv *env, id objc_object, BOOL createFlag)
{
  if (objc_tree)
    {
      jobject_id pattern;
      jobject_id *result; 
      
      pattern.objc_object = objc_object;
      result = avl_find (objc_tree, &pattern);

      if (!result && createFlag)
        {
          Class class = getClass (objc_object);
          jclass javaClass;

          if (getBit (class->info, _CLS_DEFINEDCLASS))
            {
              Class_s *nextPhase = ((BehaviorPhase_s *) class)->nextPhase;
              Type_c *typeImpl = [class getTypeImplemented];
              javaClass = java_class_for_typename (env,
                                                   typeImpl->name,
                                                   nextPhase == NULL);
            }
          else
            javaClass = java_class_for_typename (env, class->name, YES);
          
          result = java_directory_update (env, 
                                          java_instantiate (env, javaClass),
                                          objc_object);
        }
      return result;
    }
  else
    abort ();
}

jobject
java_directory_objc_find_java (JNIEnv *env, id objc_object, BOOL createFlag)
{
  if (objc_object)
    {
      jobject_id *obj =
        java_directory_objc_find (env, objc_object, createFlag);
      
      if (obj)
        return obj->java_object;
      else
        return NULL;
    }
  return NULL;
}

jobject_id * 
java_directory_update (JNIEnv *env, jobject java_object, id objc_object)
{
  jobject_id *data;
  jobject_id **foundptr;
  
  data = xmalloc (sizeof (jobject_id));
  data->java_object = (*env)->NewGlobalRef(env, java_object);
  data->objc_object = objc_object;
  
  foundptr = (jobject_id **) avl_probe (java_tree, data);
  (*foundptr)->objc_object = objc_object;

  foundptr = (jobject_id **) avl_probe (objc_tree, data);
  (*foundptr)->java_object = data->java_object;

  if (*foundptr != data)
      {
	  (*env)->DeleteGlobalRef (env, data->java_object);
	  XFREE (data);
      }
  return *foundptr;
}

jobject_id * 
java_directory_switchupdate (JNIEnv *env,
                             jobject old_java_object,
                             jobject new_java_object,
                             id objc_object)
{
  jobject_id old;
  jobject_id *data;
  jobject_id *found;
  
  old.java_object = (*env)->NewGlobalRef(env, old_java_object);
  old.objc_object = objc_object;
  if (!avl_delete (objc_tree, &old))
    abort ();

  if (!(found = avl_delete (java_tree, &old)))
    abort ();

  (*env)->DeleteGlobalRef (env, found->java_object);
  (*env)->DeleteGlobalRef (env, found->java_object);

  data = xmalloc (sizeof (jobject_id));
  data->objc_object = objc_object;
  data->java_object = (*env)->NewGlobalRef(env, new_java_object);

  return java_directory_update (env, new_java_object, objc_object);
}

jobject
java_directory_update_java (JNIEnv *env, jobject java_object, id objc_object)
{
  return java_directory_update (env, java_object, objc_object)->java_object;
}

jobject
java_directory_switchupdate_java (JNIEnv *env,
                                  jobject old_java_object,
                                  jobject new_java_object,
                                  id objc_object)
{
  return java_directory_switchupdate (env,
                                      old_java_object,
                                      new_java_object,
                                      objc_object)->java_object;
}

jobject
java_instantiate (JNIEnv *env, jclass clazz)
{
  return (*env)->AllocObject (env, clazz);
}

jstring
get_class_name (JNIEnv *env, jobject jobj)
{
  jclass clazz, classClass;
  jobject classObj, nameObj;
  jmethodID methodID;

  if (!(clazz = (*env)->GetObjectClass (env, jobj)))
    abort ();

  if (!(methodID = (*env)->GetMethodID (env,
                                        clazz,
                                        "getClass",
                                        "()Ljava/lang/Class;")))
    abort ();
  
  if (!(classObj = (*env)->CallObjectMethod (env, jobj, methodID)))
    abort ();
  
  if (!(classClass = (*env)->GetObjectClass (env, classObj)))
    abort ();
  
  if (!(methodID = (*env)->GetMethodID (env,
                                        classClass,
                                        "getName",
                                        "()Ljava/lang/String;")))
    abort ();
  
  if (!(nameObj = (*env)->CallObjectMethod (env, classObj, methodID)))
    abort ();

  return nameObj;
}

static jstring
get_base_class_name (JNIEnv *env, jobject jobj)
{
  jstring classNameObj = get_class_name (env, jobj);
  jsize len = (*env)->GetStringLength (env, classNameObj);
  jclass clazz;
  jmethodID methodID;
  jobject baseClassNameObj;

  if (!(clazz = (*env)->GetObjectClass (env, classNameObj)))
    abort ();

  if (!(methodID = (*env)->GetMethodID (env,
                                        clazz,
                                        "substring",
                                        "(II)Ljava/lang/String;")))
    abort ();

  if (!(baseClassNameObj = (*env)->CallObjectMethod (env,
                                                     classNameObj,
                                                     methodID,
                                                     0,
                                                     len - 4)))
    abort ();

  return baseClassNameObj;
}

jobject
java_instantiate_using (JNIEnv *env, jobject jobj)
{
  jclass clazz;
  jstring classNameObj;
  jmethodID methodID;
  jboolean copyFlag;
  const char *utf;

  if (!jobj)
    abort ();

  classNameObj = get_base_class_name (env, jobj);

  if (!(clazz = (*env)->GetObjectClass (env, classNameObj)))
    abort ();
  
  if (!(methodID = (*env)->GetMethodID (env,
                                        clazz,
                                        "concat",
                                        "(Ljava/lang/String;)Ljava/lang/String;")))
    abort ();
  if (!(classNameObj = (*env)->CallObjectMethod (env,
                                                 classNameObj,
                                                 methodID,
                                                 (*env)->NewStringUTF (env, "UImpl"))))
    abort ();
  
  utf = (*env)->GetStringUTFChars (env, classNameObj, &copyFlag);

  {
    char *className = copyFlag ? (char *) utf : strdup (utf), *p;

    for (p = className; *p; p++)
      if (*p == '.')
        *p = '/';
    if (!(clazz = (*env)->FindClass (env, className)))
      abort ();
  }
      
  if (copyFlag)
    (*env)->ReleaseStringUTFChars (env, classNameObj, utf);
  
  return java_instantiate (env, clazz);
}

int
compare_java_objects (const void *A, const void *B, void *PARAM)
{
   if (((jobject_id *) A)->java_object <
      ((jobject_id *) B)->java_object)
    return -1;

  return (((jobject_id *) A)->java_object >
	  ((jobject_id *) B)->java_object);
}

int
compare_objc_objects (const void *A, const void *B, void *PARAM)
{
  if (((jobject_id *) A)->objc_object <
      ((jobject_id *) B)->objc_object)
    return -1;

  return (((jobject_id *) A)->objc_object >
	  ((jobject_id *) B)->objc_object);
}

void
java_directory_init (JNIEnv *env,
                     jobject swarmEnvironment)

{
  jobject o_globalZone, o_uniformIntRand, o_uniformDblRand;
  jfieldID globalZoneFid, uniformIntRandFid, uniformDblRandFid;
  jclass class;
    
  jniEnv = env;
  java_tree = avl_create (compare_java_objects, NULL);
  objc_tree = avl_create (compare_objc_objects, NULL);
  
  create_class_refs (env);

  if (!(class = (*env)->GetObjectClass (env, swarmEnvironment)))
    abort ();
  
  globalZoneFid = (*env)->GetFieldID (env, class, "globalZone", 
				      "Lswarm/defobj/ZoneUImpl;");
  uniformIntRandFid = 
      (*env)->GetFieldID (env, class, "uniformIntRand",
			  "Lswarm/random/UniformIntegerDistUImpl;");  
  uniformDblRandFid =
      (*env)->GetFieldID (env, class, "uniformDblRand",
			  "Lswarm/random/UniformDoubleDistUImpl;");
  
  o_globalZone = (*env)->GetObjectField (env, swarmEnvironment, globalZoneFid);

  o_uniformIntRand = (*env)->GetObjectField (env, swarmEnvironment, 
					     uniformIntRandFid);
  o_uniformDblRand = (*env)->GetObjectField (env, swarmEnvironment, 
					     uniformDblRandFid);    
 
  o_globalZone = (*env)->NewGlobalRef (env, o_globalZone);
  o_uniformIntRand = (*env)->NewGlobalRef (env, o_uniformIntRand);
  o_uniformDblRand = (*env)->NewGlobalRef (env, o_uniformDblRand);
 
  java_directory_update (env, o_globalZone, globalZone);
  {
    extern id uniformIntRand, uniformDblRand;

    java_directory_update (env, o_uniformIntRand, uniformIntRand);
    java_directory_update (env, o_uniformDblRand, uniformDblRand);
  }
}

void
java_directory_drop (JNIEnv *env)
{
  void destroy_func (void *data, void *param)
    {
      (*env)->DeleteGlobalRef (env, ((jobject_id *) data)->java_object);
      XFREE (data);
    }
  avl_destroy (java_tree, NULL);
  avl_destroy (objc_tree, destroy_func);
}

SEL
java_ensure_selector (JNIEnv *env, jobject jsel)
{
  jclass clazz;
  jfieldID nameFid; 
  jstring string;
  const char *utf;
  char *name, *p;
  SEL sel;
  unsigned i;
  jboolean copyFlag;
  jfieldID retTypeFid;
  jfieldID argTypesFid;
  jfieldID objcFlagFid;
  jclass retType;
  jboolean objcFlag;
  jarray argTypes;
  jsize argCount;


  clazz = (*env)->GetObjectClass (env, jsel);
  if (!(nameFid = (*env)->GetFieldID (env, clazz, "signature", "Ljava/lang/String;")))
    abort ();
  if (!(retTypeFid = (*env)->GetFieldID (env, clazz, "retType", "Ljava/lang/Class;")))
    abort ();
  if (!(argTypesFid = (*env)->GetFieldID (env, clazz, "argTypes", "[Ljava/lang/Class;")))
    abort ();
      
  if (!(objcFlagFid = (*env)->GetFieldID (env, clazz, "objcFlag", "Z")))
    abort ();

  retType = (*env)->GetObjectField (env, jsel, retTypeFid);
  objcFlag = (*env)->GetBooleanField (env, jsel, objcFlagFid);
  argTypes = (*env)->GetObjectField (env, jsel, argTypesFid);
  argCount = (*env)->GetArrayLength (env, argTypes);
  string = (*env)->GetObjectField (env, jsel, nameFid);
  utf = (*env)->GetStringUTFChars (env, string, &copyFlag);

  if (objcFlag)
    {
      p = name = (copyFlag ? (char *) utf : strdup (utf));
      while (*p)
        {
          if (*p == '$')
            *p = ':';
          p++;
        }
    }
  else
    {
      name = xmalloc (strlen (utf) + argCount + 1);
      p = stpcpy (name, utf);
      for (i = 0; i < argCount; i++)
        *p++ = ':';
      *p = '\0';
    }

  sel = sel_get_any_typed_uid (name);

  if (!sel)
    {
      jsize ti;
      char signatureBuf[(argCount + 2) * 6], *p = signatureBuf;
      size_t pos = 0;

      size_t alignto (size_t pos, size_t alignment)
        {
          size_t mask = (alignment - 1);
          
          if ((pos & mask) == 0)
            return pos;
          else
            return (pos + alignment) & ~mask;
        }

      void addstr (char type, BOOL regFlag, unsigned pos)
        {
          *p++ = type;
          if (regFlag)
            *p++ = '+';
          {
            char posbuf[4];
            
            sprintf (posbuf, "%u", pos);
            p = stpcpy (p, posbuf);
          }
        }
      void add (jobject class, BOOL regFlag)
        {
          char type;
          size_t size;

          jboolean classp (jclass matchClass)
            {
              return (*env)->IsSameObject (env, class, matchClass);
            }
          
          if (classp (c_object))
            {
              type = _C_ID;
              pos = alignto (pos, __alignof__ (id));
              size = sizeof (id);
            }
          else if (classp (c_string))
            {
              type = _C_CHARPTR;
              pos = alignto (pos, __alignof__ (const char *));
              size = sizeof (const char *);
            }
          else if (classp (c_int))
            {
              type = _C_INT;
              pos = alignto (pos, __alignof__ (int));
              size = sizeof (int);
            }
          else if (classp (c_short))
            {
              type = _C_SHT;
              pos = alignto (pos, __alignof__ (short));
              size = sizeof (short);
            }
          else if (classp (c_long))
            {
              type = _C_LNG;
              pos = alignto (pos, __alignof__ (long));
              size = sizeof (long);
            }
          else if (classp (c_boolean))
            {
              type = _C_UCHR;
              pos = alignto (pos, __alignof__ (BOOL));
              size = sizeof (BOOL);
            }
          else if (classp (c_byte))
            {
              type = _C_UCHR;
              pos = alignto (pos, __alignof__ (unsigned char));
              size = sizeof (unsigned char);
            }
          else if (classp (c_char))
            {
              type = _C_CHR;
              pos = alignto (pos, __alignof__ (char));
              size = sizeof (char);
            }
          else if (classp (c_float))
            {
              type = _C_FLT;
              pos = alignto (pos, __alignof__ (float));
              size = sizeof (float);
            }
          else if (classp (c_double))
            {
              type = _C_DBL;
              pos = alignto (pos, __alignof__ (double));
              size = sizeof (double);
            }
          else if (classp (c_void))
            {
              type = _C_VOID;
              size = 0;
            }
          else
            abort ();
          addstr (type, regFlag, pos);
          pos += size;
        }
      
      add (retType, NO);
#if 0
      pos = alignto (pos, __alignof__ (id));
#else
      pos = 8;
#endif
      addstr (_C_ID, YES, pos);
      pos += sizeof (id);
      pos = alignto (pos, __alignof__ (SEL));
      addstr (_C_SEL, YES, pos);
      pos += sizeof (SEL);

      for (ti = 0; ti < argCount; ti++)
        add ((*env)->GetObjectArrayElement (env, argTypes, ti), YES);

      printf ("[%s][%s]\n", name, signatureBuf);
      sel = sel_register_typed_name (name, signatureBuf);
    }
  java_directory_update (env, jsel, (id) sel);

  if (copyFlag)
    (*env)->ReleaseStringUTFChars (env, string, utf);
  else
    XFREE (name);
  return sel;
}

Class
java_ensure_class (JNIEnv *env, jclass javaClass)
{
  Class objcClass;
  jstring name = get_class_name (env, (*env)->AllocObject (env, javaClass));
  
  {
    jboolean isCopy;
    const char *className =
      (*env)->GetStringUTFChars (env, name, &isCopy);
    
    objcClass = objc_lookup_class (className);
    (*env)->ReleaseStringUTFChars (env, name, className);
  }
  java_directory_update (env, (jobject) javaClass, (id) objcClass);
  return objcClass;
}

const char *
java_copy_string (JNIEnv *env, jstring javaString)
{
  jboolean isCopy;
  const char *str = (*env)->GetStringUTFChars (env, javaString, &isCopy);
  const char *ret = strdup (str);

  if (isCopy)
    (*env)->ReleaseStringUTFChars (env, javaString, str);
  return ret;
}


void
java_cleanup_strings (JNIEnv *env, const char **stringArray, size_t count)
{
  size_t i;

  for (i = 0; i < count; i++)
    XFREE (stringArray[i]);
}
#endif
