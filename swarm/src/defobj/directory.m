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

jclass c_boolean,
  c_char, c_byte,
  c_int, 
  c_short, c_long,
  c_float, c_double,
  c_object, c_string, 
  c_void;

jclass c_Boolean, 
  c_Char, c_Byte, 
  c_Integer, c_Short,
  c_Long, c_Float,
  c_Double;
  
jclass c_field, c_class, c_method, c_Selector;

jmethodID m_BooleanValueOf,
  m_ByteValueOf, 
  m_IntegerValueOf, 
  m_ShortValueOf, m_LongValueOf,   
  m_FloatValueOf, m_DoubleValueOf, 
  m_StringValueOf, 
  m_FieldSet, m_FieldSetChar,
  m_ClassGetDeclaredField,
  m_ClassGetDeclaredFields,
  m_ClassGetDeclaredMethods,
  m_FieldGetType,
  m_FieldGetInt,
  m_FieldGetDouble,
  m_FieldGet,
  m_FieldGetName,
  m_MethodGetName,
  m_SelectorConstructor;  

jfieldID f_nameFid, f_retTypeFid, 
  f_argTypesFid, f_objcFlagFid;

extern id ControlStateRunning, ControlStateStopped,
  ControlStateStepping, ControlStateQuit,ControlStateNextTime, 
  probeLibrary, probeDisplayManager, uniformIntRand, uniformDblRand;
  
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

      c_Boolean = find ("Boolean");
      c_Char = find ("Character");
      c_Byte= find ("Byte");
      c_Integer = find ("Integer");
      c_Short = find ("Short");
      c_Long = find ("Long");
      c_Float = find("Float");
      c_Double = find ("Double");
     
      c_string = find ("String");
      c_object = find ("Object");
      c_class = find ("Class");

      if (!(c_field = (*env)->FindClass (env, "java/lang/reflect/Field")))
        abort ();
      c_field = (*env)->NewGlobalRef (env, c_field);

      if (!(c_method = (*env)->FindClass (env, "java/lang/reflect/Method")))
        abort ();
      c_method = (*env)->NewGlobalRef (env, c_method);

      if (!(c_Selector = (*env)->FindClass (env, "swarm/Selector")))
        abort ();
      c_Selector = (*env)->NewGlobalRef (env, c_Selector);

      initFlag = YES;
    }
}

void 
create_method_refs (JNIEnv *env)
{
  jmethodID findMethodID (const char *name, jclass clazz)
    {
      char sig[31 + strlen (name) + 2];
      char *p;

      jmethodID res;
      p = stpcpy (sig, "(Ljava/lang/String;)Ljava/lang/");
      p = stpcpy (p, name);
      p = stpcpy (p, ";");

      if (!(res = (*env)->GetStaticMethodID (env, clazz, "valueOf", sig)))
	abort ();

      return res;
    }   
  
  m_BooleanValueOf = findMethodID ("Boolean", c_Boolean);
  
  m_ByteValueOf = findMethodID ("Byte", c_Byte);
  
  m_IntegerValueOf = findMethodID ("Integer", c_Integer);
  
  m_ShortValueOf = findMethodID ("Short", c_Short);
  
  m_LongValueOf = findMethodID ("Long", c_Long);
  
  m_FloatValueOf = findMethodID ("Float", c_Float);
  
  m_DoubleValueOf = findMethodID ("Double", c_Double);
  
  if (!(m_StringValueOf = 
      (*env)->GetStaticMethodID (env, c_string, "valueOf", 
				 "(Ljava/lang/Object;)Ljava/lang/String;")))
    abort ();

  if (!(m_FieldSet = 
	(*env)->GetMethodID (env, c_field, "set", 
			     "(Ljava/lang/Object;Ljava/lang/Object;)V")))
    abort();

  if (!(m_FieldSetChar = 
	(*env)->GetMethodID (env, c_field, "setChar", 
			     "(Ljava/lang/Object;C)V")))
    abort();
 
  if (!(m_ClassGetDeclaredField =
      (*env)->GetMethodID (env, c_class, "getDeclaredField",
			   "(Ljava/lang/String;)Ljava/lang/reflect/Field;")))
    abort();

  if (!(m_ClassGetDeclaredFields =
  	(*env)->GetMethodID (env, c_class, "getDeclaredFields",
			     "()[Ljava/lang/reflect/Field;")))
    abort();
  
  if (!(m_ClassGetDeclaredMethods =
  	(*env)->GetMethodID (env, c_class, "getDeclaredMethods",
  		     "()[Ljava/lang/reflect/Method;")))
    abort();

  if (!(m_FieldGetName = 
	(*env)->GetMethodID (env, c_field, "getName", "()Ljava/lang/String;")))
    abort();

  if (!(m_FieldGetType =
	(*env)->GetMethodID (env, c_field, "getType", "()Ljava/lang/Class;")))
    abort();
  
  if (!(m_FieldGetInt =
      (*env)->GetMethodID (env, c_field, "getInt", 
			   "(Ljava/lang/Object;)I")))
    abort();

  if (!(m_FieldGetDouble =
      (*env)->GetMethodID (env, c_field, "getDouble", 
			   "(Ljava/lang/Object;)D")))
    abort();

  if (!(m_FieldGet =
      (*env)->GetMethodID (env, c_field, "get",
			   "(Ljava/lang/Object;)Ljava/lang/Object;")))
    abort();

  if (!(m_MethodGetName =
	(*env)->GetMethodID (env, c_method, "getName",
			     "()Ljava/lang/String;")))
    abort();
  
  if (!(m_SelectorConstructor =
	(*env)->GetMethodID (env, c_Selector, "<init>", 
			     "(Ljava/lang/Class;Ljava/lang/String;Z)V")))
    abort();
}


void
create_field_refs (JNIEnv * env)
{

  if (!(f_nameFid = (*env)->GetFieldID (env, c_Selector, "signature", "Ljava/lang/String;")))
    abort ();
  if (!(f_retTypeFid = (*env)->GetFieldID (env, c_Selector, "retType", "Ljava/lang/Class;")))
    abort ();
  if (!(f_argTypesFid = (*env)->GetFieldID (env, c_Selector, "argTypes", "[Ljava/lang/Class;")))
    abort ();
  if (!(f_objcFlagFid = (*env)->GetFieldID (env, c_Selector, "objcFlag", "Z")))
    abort ();


}


static jclass
java_class_for_typename (JNIEnv *env, const char *typeName, BOOL usingFlag)
{
  extern const char *swarm_lookup_module (const char *name);
  const char *module = swarm_lookup_module (typeName);
  size_t modulelen = module ? strlen (module) + 1 : 0;
  char javaClassName[5 + 1 + modulelen + strlen (typeName) + 5 + 1];
  char *p;

  p = stpcpy (javaClassName, "swarm/");
  if (module)
    {
      p = stpcpy (p, module);
      p = stpcpy (p, "/");
    }
  p = stpcpy (p, typeName);
  if (!usingFlag)
    p = stpcpy (p, "C");
  p = stpcpy (p, "Impl");

  return (*env)->FindClass (env, javaClassName);
}

static Class
objc_class_for_classname (const char * classname)
{
  int len = strlen (classname);
  int end, beg;
  char typename[len];
  
  if (!strcmp ("Impl", classname + len - 4))
    {
      int j = 0;
      if (*(classname + len - 5) == 'C')
	end = len - 5;
      else
	end = len - 4;
      for (beg=0; (beg < end && j < 2); beg++)
	if (classname[beg] == '.') j++;
      if (j==2)
	{
	  len = end - beg;
	  strncpy (typename, &(classname[beg]), len);
	  typename[len] = 0;
	  return objc_lookup_class (typename);
	}
    }
    return objc_lookup_class (classname);
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

              Type_c *typeImpl;
	      Class_s *nextPhase;
	      nextPhase= ((BehaviorPhase_s *) class)->nextPhase;
              typeImpl = [class getTypeImplemented];
	      javaClass = java_class_for_typename (env,
						   typeImpl->name,
						   nextPhase == NULL); 
            }
          else
	    {
	      Type_c *typeImpl;
	      typeImpl = [class getTypeImplemented];
	      if (typeImpl)
		  javaClass = java_class_for_typename (env,
						       typeImpl->name,
						       YES);
	       else 
		   javaClass = java_class_for_typename (env, 
						      [[objc_object getClass]
							getName],
						      YES);
	    }
          result = java_directory_update (env, 
                                          java_instantiate (env, javaClass),
                                          objc_object);
        }
      return result;
    }
  else
    return NULL;
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

  if (*foundptr != data)
    {	
      (*env)->DeleteGlobalRef (env, data->java_object);
      XFREE (data);
    }
  else
    {
      foundptr = (jobject_id **) avl_probe (objc_tree, data);
      (*foundptr)->java_object = data->java_object;
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
  jobject_id *found;

  old.java_object = (*env)->NewGlobalRef(env, old_java_object);
  old.objc_object = objc_object;
  if (!avl_delete (objc_tree, &old))
    abort ();

  if (!(found = avl_delete (java_tree, &old)))
    abort ();

  (*env)->DeleteGlobalRef (env, found->java_object);
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
  jmethodID mid;
  jobject value;
  mid = (*env)->GetMethodID (env, clazz, "<init>","()V");
  value = (*env)->AllocObject (env, clazz);
  (*env)->CallVoidMethod (env, value, mid);
  return value;

}

static jstring
get_class_name_from_class_object (JNIEnv *env, jobject classObj)
{
  jmethodID methodID;
  jobject nameObj;
  jclass class;
  
  if (!(class = (*env)->GetObjectClass (env, classObj)))
    abort ();

  if (!(methodID = (*env)->GetMethodID (env,
                                        class,
                                        "getName",
                                        "()Ljava/lang/String;")))
    abort ();
  
  if (!(nameObj = (*env)->CallObjectMethod (env, classObj, methodID)))
    abort ();
  
  return nameObj;
}

static jstring
get_class_name_from_object (JNIEnv *env, jobject jobj)
{
  jclass class;
  jmethodID methodID;
  jobject classObj;

  if (!(class = (*env)->GetObjectClass (env, jobj)))
    abort ();

  if (!(methodID = (*env)->GetMethodID (env,
                                        class,
                                        "getClass",
                                        "()Ljava/lang/Class;")))
    abort ();
  
  if (!(classObj = (*env)->CallObjectMethod (env, jobj, methodID)))
    abort ();

  return get_class_name_from_class_object (env, classObj);
}

static jstring
get_class_name (JNIEnv *env, jclass class)
{
  return get_class_name_from_object (env,
                                     (*env)->AllocObject (env, class));
}

#if 0
static jstring
get_base_class_name (JNIEnv *env, jobject jobj)
{
  jstring classNameObj = get_class_name_from_object (env, jobj);
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
                                                     len - 5)))
    abort ();

  return baseClassNameObj;
}
#endif

static void
fill_signature (char *buf, const char *className)
{
  char *bp = buf;
  const char *cp = className;

  *bp++ = 'L';
  while (*cp)
    {
      *bp = (*cp == '.') ? '/' : *cp;
      bp++;
      cp++;
    }
  *bp++ = ';';
  *bp = '\0';
}

static const char *
create_signature_from_class_name (JNIEnv *env, const char *className)
{
  char buf[1 + strlen (className) + 1 + 1];

  fill_signature (buf, className);
  return strdup (buf);
}

static const char *
create_signature_from_object (JNIEnv *env, jobject jobj)
{
  const char *className =
    java_copy_string (env, get_class_name_from_object (env, jobj));
  const char *ret = create_signature_from_class_name (env, className);

  XFREE (className);
  return ret;
}

jobject
java_next_phase (JNIEnv *env, jobject jobj)
{
  jclass class = java_class_for_typename (env, "Phase", NO);
  jfieldID fid;
  const char *sig =
    create_signature_from_class_name (env, "java.lang.Object");

  if (!(fid = (*env)->GetFieldID (env, class, "nextPhase", sig)))
    abort ();
  XFREE (sig);
  return (*env)->GetObjectField(env, jobj, fid);
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
  jclass c_SwarmEnvironment;
  void setFieldInSwarm(char  *className,  char  *fieldName, id objcObject)
    {
      jclass clazz;
      jfieldID fid;
      jobject value;
      const char *sig;

      clazz = java_class_for_typename (env, className, YES);
      value = (*env)->AllocObject (env, clazz);
      sig = create_signature_from_object (env, value);
      fid = (*env)->GetFieldID (env, c_SwarmEnvironment, fieldName, sig);
      (*env)->SetObjectField (env,
			      swarmEnvironment,
			      fid, value);	
     
      java_directory_update (env, value, objcObject);
    }
  jniEnv = env;
  java_tree = avl_create (compare_java_objects, NULL);
  objc_tree = avl_create (compare_objc_objects, NULL);
  
  create_class_refs (env);
  create_method_refs (env);
  create_field_refs (env);

  if (!(c_SwarmEnvironment = (*env)->GetObjectClass (env, swarmEnvironment)))
    abort ();

  setFieldInSwarm ("ProbeLibrary", "probeLibrary", probeLibrary);
  setFieldInSwarm ("Zone", "globalZone", globalZone);
  setFieldInSwarm ("UniformIntegerDist",  "uniformIntRand", uniformIntRand);
  setFieldInSwarm ("UniformDoubleDist", "uniformDblRand", uniformDblRand);
  setFieldInSwarm ("Symbol", "ControlStateRunning", ControlStateRunning);
  setFieldInSwarm ("Symbol", "ControlStateStopped", ControlStateStopped);
  setFieldInSwarm ("Symbol", "ControlStateStepping", ControlStateStepping);
  setFieldInSwarm ("Symbol", "ControlStateQuit", ControlStateQuit);
  setFieldInSwarm ("Symbol", "ControlStateNextTime", ControlStateNextTime);
  setFieldInSwarm ("ProbeDisplayManager","probeDisplayManager", 
		   probeDisplayManager);
  

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
  jstring string;
  const char *utf;
  char *name, *p;
  SEL sel;
  unsigned i;
  jboolean copyFlag;
  jclass retType;
  jboolean objcFlag;
  jarray argTypes;
  jsize argCount;

  if (!jsel) return NULL;
  retType = (*env)->GetObjectField (env, jsel, f_retTypeFid);
  objcFlag = (*env)->GetBooleanField (env, jsel, f_objcFlagFid);
  argTypes = (*env)->GetObjectField (env, jsel, f_argTypesFid);
  argCount = (*env)->GetArrayLength (env, argTypes);
  string = (*env)->GetObjectField (env, jsel, f_nameFid);
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

    {
      jsize ti;
      char signatureBuf[(argCount + 3) * 2 + 1], *p = signatureBuf;

      void add_type (char type)
        {
          *p++ = type;
          *p++ = '0';
          *p = '\0';
        }
      void add (jobject class)
        {
          char type;

          jboolean classp (jclass matchClass)
            {
              return (*env)->IsSameObject (env, class, matchClass);
            }
          
          if (classp (c_object))
            type = _C_ID;
          else if (classp (c_string))
            type = _C_CHARPTR;
          else if (classp (c_int))
            type = _C_INT;
          else if (classp (c_short))
            type = _C_SHT;
          else if (classp (c_long))
            type = _C_LNG;
          else if (classp (c_boolean))
            type = _C_UCHR;
          else if (classp (c_byte))
            type = _C_UCHR;
          else if (classp (c_char))
            type = _C_CHR;
          else if (classp (c_float))
            type = _C_FLT;
          else if (classp (c_double))
            type = _C_DBL;
          else if (classp (c_void))
            type = _C_VOID;
          else
            type = _C_ID;
          add_type (type);
        }
      
      add (retType);
      add_type (_C_ID);
      add_type (_C_SEL);

      for (ti = 0; ti < argCount; ti++)
        add ((*env)->GetObjectArrayElement (env, argTypes, ti));
      
      sel = sel_get_any_typed_uid (name);

      if (sel)
	{
	  if (!(sel_get_typed_uid (name, signatureBuf)))
	    raiseEvent (SourceMessage, "Method '%s' has different type from the Swarm library method with the same \n  name! Adjust type to match Swarm method's type or use different method name!\n", name);
	}
      else
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
  jstring name = get_class_name (env, javaClass);
  jboolean isCopy;
  const char *className =
      (*env)->GetStringUTFChars (env, name, &isCopy);
  objcClass = objc_class_for_classname (className);
  
  if (isCopy)
    (*env)->ReleaseStringUTFChars (env, name, className);

  // if the corresponding class does not exist create new Java Proxy

  if (objcClass == nil)
      if (!(objcClass = java_directory_java_find_objc (env, javaClass)))
	  objcClass = [JavaProxy create: globalZone];

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

Class 
java_get_class_from_objc_object (id object)
{
  jobject jobj;
  jclass jcls;
  Class result;
  id proxy;
  jboolean isCopy;
  const char * classname;

  if ((jobj = java_directory_objc_find_java (jniEnv, object, NO)))
    {
      jstring javaclassname;
      jcls = (*jniEnv)->GetObjectClass (jniEnv,jobj);
      javaclassname = get_class_name_from_class_object (jniEnv, jcls);
      classname = 
	(*jniEnv)->GetStringUTFChars (jniEnv, javaclassname, &isCopy);
      result = objc_class_for_classname (classname);
      if (isCopy)
	(*jniEnv)->ReleaseStringUTFChars (jniEnv, javaclassname, classname);
      if (result)
	return result;

      if ((proxy = java_directory_java_find_objc (jniEnv, jcls)))
	return proxy;
      else
	return java_ensure_class (jniEnv, jcls);
    }
  else
    return [object getClass];
}
 
Class 
java_get_swarm_class (id object)
{
  jobject jobj;
  jclass jcls;
  Class result;
  id proxy;
  jboolean isCopy;
  const char * classname;
  
  if ((jobj = java_directory_objc_find_java (jniEnv, object, NO)))
    {
      jstring javaclassname;
      jcls = (*jniEnv)->GetObjectClass (jniEnv,jobj);
      javaclassname = get_class_name_from_class_object (jniEnv, jcls);
      classname = 
	(*jniEnv)->GetStringUTFChars (jniEnv, javaclassname, &isCopy);
      result = objc_class_for_classname (classname);
      if (isCopy)
	(*jniEnv)->ReleaseStringUTFChars (jniEnv, javaclassname, classname);
      if (result)
	return result;      
      if ((proxy = java_directory_java_find_objc (jniEnv, jcls)))
	return proxy;
       else
	 return java_ensure_class (jniEnv, jcls);
    }
  else
    return [object getClass];
}

#endif



