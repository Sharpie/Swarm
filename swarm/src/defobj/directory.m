]#import <defobj/directory.h>

#import <defobj/DefObject.h>
#import <defobj/defalloc.h>

#include <misc.h>
#include <misc/avl.h>
#include <objc/objc.h>
#include <objc/objc-api.h>
#include <objc/mframe.h> // mframe_build_signature

#import "JavaProxy.h"
#import <defobj.h>

#import <defobj/Program.h> // Type_c
#import <collections.h> // Map

#define extern
#ifdef HAVE_JDK
#import "javavars.h"
#endif
#undef extern

#ifndef DISABLE_ZONES
#define DUPCLASSNAME(str) SSTRDUP(str)
#define FREECLASSNAME(str) SFREEBLOCK(str)
#else
#define DUPCLASSNAME(str) xstrdup (str)
#define FREECLASSNAME(str) XFREE (str)
#undef SFREEBLOCK
#define SFREEBLOCK(mem)
#undef STRDUP
#define STRDUP(str) xstrdup (str)
#undef SSTRDUP
#define SSTRDUP (str) xstrdup (str)
#endif

extern void *alloca (size_t);

#define internalimplementation implementation // defeat make-h2x

#ifdef HAVE_JDK

BOOL initFlag = NO;

extern JNIEnv *jniEnv;

id swarmDirectory;

#if 1
#define DIRECTORY_SIZE 21599
#else
#define DIRECTORY_SIZE 263009
#endif

static const char *
getObjcName (jobject javaObject, id object)
{
  if ((*jniEnv)->IsInstanceOf (jniEnv, javaObject, c_Selector))
    return object ? sel_get_name ((SEL) object) : "M(<nil>)";
  else
    return object ? [object name] : "nil";
}

static const char *
get_class_name (JNIEnv *env, jclass class)
{
  jobject string;
  const char *ret;

  if (!(string = (*env)->CallObjectMethod (env, class, m_ClassGetName)))
    abort ();

  ret = swarm_directory_copy_java_string (env, string);
  (*env)->DeleteLocalRef (env, string);
  return ret;
}

const char *
swarm_directory_java_class_name (JNIEnv *env, jobject obj)
{
  jclass class;
  const char *ret;

  if (!(class = (*env)->GetObjectClass (env, obj)))
    abort ();

  ret = get_class_name (env, class);
  (*env)->DeleteLocalRef (env, class);
  return ret;
}

unsigned
swarm_directory_java_hash_code (JNIEnv *env, jobject javaObject)
{
  int hashCode;

  hashCode = (*env)->CallIntMethod (env, javaObject, m_HashCode);
  return (hashCode < 0 ? - hashCode : hashCode) % DIRECTORY_SIZE;
}

@internalimplementation DirectoryEntry
- setJavaObject: (jobject)theJavaObject
{
  javaObject = theJavaObject;
  return self;
}

- setObject: theObject
{
  object = theObject;
  return self;
}

- (const char *)getObjcName
{
  return getObjcName (javaObject, object);
}

- (unsigned)getHashCode
{
  return swarm_directory_java_hash_code (jniEnv, javaObject);
}

- (int)compare: obj
{
  int ret;
  jobject objJavaObject = ((DirectoryEntry *) obj)->javaObject;

#if 0
  printf ("`%s'%p/%p vs %p\n",
          [self getObjcName], object, javaObject,
          ((DirectoryEntry *) obj)->javaObject);
#endif
  ret = (int) !(*jniEnv)->CallBooleanMethod (jniEnv,
                                             javaObject,
                                             m_Equals,
                                             objJavaObject);
                                             
  return ret;
}

- (void)drop
{
  (*jniEnv)->DeleteGlobalRef (jniEnv, javaObject);
  [getZone (self) freeIVars: self];
}

- (void)describe: outputCharStream
{
  const char *className =
    swarm_directory_java_class_name (jniEnv, javaObject);
  
  [outputCharStream catPointer: self];
  [outputCharStream catC: " objc: "];
  [outputCharStream catC: [self getObjcName]];
  [outputCharStream catC: " "];
  [outputCharStream catPointer: object];
  [outputCharStream catC: "  java: "];
  [outputCharStream catC: className];
  [outputCharStream catC: " "];
  [outputCharStream catPointer: javaObject];
  [outputCharStream catC: "\n"];
  FREECLASSNAME (className);
}

@end

static int
compare_objc_objects (const void *A, const void *B, void *PARAM)
{
  if (((DirectoryEntry *) A)->object <
      ((DirectoryEntry *) B)->object)
    return -1;

  return (((DirectoryEntry *) A)->object >
	  ((DirectoryEntry *) B)->object);
}

#define ENTRY(theObject,theJavaObject) [[[[DirectoryEntry createBegin: globalZone] setJavaObject: theJavaObject] setObject: theObject] createEnd]
#define OBJCENTRY(theObject) ENTRY(theObject,0)
#define JAVAENTRY(theJavaObject) ENTRY(0,theJavaObject)
#define JAVAFINDENTRY(theJavaObject) ({ DirectoryEntry *_findEntry  = alloca (sizeof (DirectoryEntry)); _findEntry->javaObject = theJavaObject; _findEntry; })
#define OBJCFINDENTRY(theObject) ({ DirectoryEntry *_findEntry  = alloca (sizeof (DirectoryEntry)); _findEntry->object = theObject; _findEntry; })

@internalimplementation Directory
+ createBegin: aZone
{
  Directory *obj = [super createBegin: aZone];
  size_t size = sizeof (id) * DIRECTORY_SIZE;

  obj->table = [aZone alloc: size];
  memset (obj->table, 0, size);
  obj->objc_tree = avl_create (compare_objc_objects, NULL);
  obj->findEntry = ENTRY (0, 0);
  return obj;
}

- javaFind: (jobject)theJavaObject
{
  if (theJavaObject)
    {
      id ret;
      unsigned index = swarm_directory_java_hash_code (jniEnv, theJavaObject);
      id <Map> m = table[index];
      
      findEntry->javaObject = theJavaObject;
      ret = m ? [m at: findEntry] : nil;
      return ret;
    }
  return nil;
}

- javaFindObjc: (jobject)theJavaObject
{
  DirectoryEntry *entry = [self javaFind: theJavaObject];

  return entry ? entry->object : nil;
}

- objcFind: theObject
{
  if (theObject)
    {
      DirectoryEntry *ret;
      
      ret = avl_find (objc_tree, OBJCFINDENTRY (theObject));
      return ret;
    }
  return nil;
}

- (jobject)objcFindJava: theObject
{
  DirectoryEntry *entry = [self objcFind: theObject];

  return entry ? entry->javaObject : NULL;
}

- add: theObject javaObject: (jobject)lref
{
  unsigned index;
  id <Map> m;
  DirectoryEntry *entry;
  jobject javaObject = (*jniEnv)->NewGlobalRef (jniEnv, lref);
  
  entry = ENTRY (theObject, javaObject);
  index = swarm_directory_java_hash_code (jniEnv, javaObject);
  m = table[index];

  if (m == nil)
    {
      m = [Map create: getZone (self)];
      table[index] = m;
    }
  [m at: entry insert: entry];
  avl_probe (objc_tree, entry);
  return entry;
}

- (void)switchJavaEntry: (DirectoryEntry *)entry javaObject: (jobject)theJavaObject
{
  unsigned index;
  id <Map> m;
  
  theJavaObject = (*jniEnv)->NewGlobalRef (jniEnv, theJavaObject);
  index = swarm_directory_java_hash_code (jniEnv, entry->javaObject);
  m = table[index];
  [m remove: entry];
  (*jniEnv)->DeleteGlobalRef (jniEnv, entry->javaObject);
  
  index = swarm_directory_java_hash_code (jniEnv, theJavaObject);
  entry->javaObject = theJavaObject;
  if (!table[index])
    table[index] = [Map create: getZone (self)];
  [table[index] at: entry insert: entry];
}

- switchJava: theObject javaObject: (jobject)theJavaObject
{
  DirectoryEntry *entry;
  
  if (!(entry = avl_find (objc_tree, OBJCFINDENTRY (theObject))))
    abort ();
  [self switchJavaEntry: entry javaObject: theJavaObject];
  return (id) entry;
}

- nextPhase: nextPhase currentJavaPhase: (jobject)currentJavaPhase
{
  jobject nextJavaPhase = SD_NEXTJAVAPHASE (jniEnv, currentJavaPhase);
  id currentPhase = SD_FINDOBJC (jniEnv, currentJavaPhase);
  DirectoryEntry *retEntry;
  
  if (currentPhase != nextPhase)
    {
      id entry = ENTRY (nextPhase, currentJavaPhase);
      DirectoryEntry **entryptr = 
        (DirectoryEntry **) avl_probe (objc_tree, entry);

      if (*entryptr != entry)
        abort ();

      [self switchJavaEntry: entry javaObject: nextJavaPhase];
      {
        id ret;
        
        ret = avl_delete (objc_tree, OBJCFINDENTRY (currentPhase));
        if (!ret)
          abort ();
        [ret drop];
      }
      retEntry = *entryptr;
    }
  else
    {
      DirectoryEntry *entry = avl_find (objc_tree, OBJCFINDENTRY (nextPhase));
      
      if (!entry)
        abort ();

      [self switchJavaEntry: entry javaObject: nextJavaPhase];
      
      retEntry = entry;
    }
  (*jniEnv)->DeleteLocalRef (jniEnv, nextJavaPhase);
  return retEntry;
}

- switchObjc: theObject javaObject: (jobject)theJavaObject
{
  DirectoryEntry *entry;
  unsigned index;
  id <Map> m;
  
  index = swarm_directory_java_hash_code (jniEnv, theJavaObject);
  m = table[index];
  entry = [table[index] at: JAVAFINDENTRY (theJavaObject)];
  if (!entry)
    abort ();
  
  if (!avl_delete (objc_tree, entry))
    abort ();
  entry->object = theObject;

  {
    void **foundEntry;
    
    foundEntry = avl_probe (objc_tree, entry);
    if (*foundEntry != entry)
      abort ();
  }
  return entry;
}

- javaEnsureObjc: (jobject)javaObject
{
  if (!javaObject)
    return nil;
  else
    {
      DirectoryEntry *result; 
      
      if (!javaObject)
        return nil;
  
      result = [swarmDirectory javaFind: javaObject];
      
      if ((*jniEnv)->IsInstanceOf (jniEnv, javaObject, c_String))
        {
          jboolean isCopy;
          const char *utf, *str;
          
          utf = (*jniEnv)->GetStringUTFChars (jniEnv, javaObject, &isCopy);
          str = STRDUP (utf);
          if (isCopy)
            (*jniEnv)->ReleaseStringUTFChars (jniEnv, javaObject, utf);
          
          if (result)
            {
              const char *last = (const char *) result->object;

              result = SD_SWITCHOBJC (jniEnv, javaObject, (id) str);
	      FREEBLOCK ((void *) last);
            }
          else
            result = SD_ADD (jniEnv, javaObject, (id) str);
        }
      else if (!result) 
        result = SD_ADD (jniEnv, javaObject, [JavaProxy create: globalZone]);
      
      return result->object;
    }
}

static const char *
java_classname_for_typename (JNIEnv *env, const char *typeName, BOOL usingFlag)
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
  return DUPCLASSNAME (javaClassName);
}

static const char *
objcFindJavaClassName (Class class)
{
  const char *javaClassName;
  
  if (getBit (class->info, _CLS_DEFINEDCLASS))
    {
      Type_c *typeImpl;
      Class_s *nextPhase;

      nextPhase = ((BehaviorPhase_s *) class)->nextPhase;
      typeImpl = [class getTypeImplemented];
      javaClassName = java_classname_for_typename (jniEnv,
						   typeImpl->name,
						   nextPhase == NULL); 
    }
  else
    {
      Type_c *typeImpl;
      typeImpl = [class getTypeImplemented];
      
      if (typeImpl)
        javaClassName =
	  java_classname_for_typename (jniEnv, typeImpl->name, YES);
      else 
        javaClassName =
	  java_classname_for_typename (jniEnv, class->name, YES);
    }
  return javaClassName;
}

- (jclass)objcFindJavaClass: (Class)class
{
  const char *javaClassName = objcFindJavaClassName (class);
  jclass ret = (*jniEnv)->FindClass (jniEnv, javaClassName);

  FREECLASSNAME (javaClassName);
  return ret;
}

- (jobject)objcEnsureJava: object
{
  DirectoryEntry *result; 

  if (!object)
    return 0;

  result = [swarmDirectory objcFind: object];
  
  if (!result)
    {
      Class class = getClass (object);
      jclass javaClass = [self objcFindJavaClass: class];
      jobject lref = swarm_directory_java_instantiate (jniEnv, javaClass);
      
      result = SD_ADD (jniEnv, lref, object);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, javaClass);
    }
  return result->javaObject;
}

- (BOOL)objcRemove: object
{
  DirectoryEntry *entry = [swarmDirectory objcFind: object];

  if (entry)
    {
      unsigned index;
      id <Map> m;

      index = swarm_directory_java_hash_code (jniEnv, entry->javaObject);
      m = table[index];
      if (!m)
        abort ();
      {
        DirectoryEntry *ret;

        ret = [m remove: entry];

        if (ret != entry)
          raiseEvent (WarningMessage, "remove (%p) != %p\n", entry, ret);

        ret = avl_delete (objc_tree, entry);
        
        if (ret != entry)
          abort ();
      }
      [entry drop];
      return YES;
    }
  
  return NO;
}

- (void)describe: outputCharStream
{
  unsigned i;

  for (i = 0; i < DIRECTORY_SIZE; i++)
    {
      if (table[i])
        {
          [outputCharStream catC: "["];
          [outputCharStream catUnsigned: i];
          [outputCharStream catC: "]:\n"];
          xfprint (table[i]);
        }
    }
}

@end

static jclass
get_java_class (JNIEnv *env, const char *name)
{
  char class_name_buf[10 + strlen (name) + 1];
  char *p;
  jclass ret, clazz;
  
  p = stpcpy (class_name_buf, "java/lang/");
  p = stpcpy (p, name);
  if (!(clazz = (*env)->FindClass (env, class_name_buf)))
    abort ();
  
  ret = (*env)->NewGlobalRef (env, clazz);
  (*env)->DeleteLocalRef (env, clazz);
  return ret;
}

#if 0
static jclass
get_type_field_for_class (JNIEnv *env, jclass clazz)
{
  jfieldID field;
  jclass ret;
  jobject lref;
  
  if (!(field = (*env)->GetStaticFieldID (env,
                                          clazz,
                                          "TYPE",
                                          "Ljava/lang/Class;")))
    abort ();
  if (!(lref = (*env)->GetStaticObjectField (env, clazz, field)))
    abort ();
  ret = (*env)->NewGlobalRef (env, lref);
  (*env)->DeleteLocalRef (env, lref);
  return ret;
}
#endif

static void
create_bootstrap_refs (JNIEnv *env)
{
  jclass lref;

  if (!(lref = (*env)->FindClass (env, "swarm/Primitives")))
    abort ();
  c_Primitives = (*env)->NewGlobalRef (env, lref);
  (*env)->DeleteLocalRef (env, lref);
  
  if (!(m_PrimitivesGetTypeMethod =
        (*env)->GetStaticMethodID (env, c_Primitives, "getTypeMethod",
                                   "(Ljava/lang/String;)Ljava/lang/reflect/Method;")))
    abort ();
  
  if (!(lref = (*env)->FindClass (env, "java/lang/reflect/Method")))
    abort ();
  c_Method = (*env)->NewGlobalRef (env, lref);
  (*env)->DeleteLocalRef (env, lref);

  if (!(m_MethodGetReturnType =
        (*env)->GetMethodID (env, c_Method, "getReturnType",
                             "()Ljava/lang/Class;")))
    abort();
}

static void
create_class_refs (JNIEnv *env)
{
  jobject lref;

  jclass get_primitive (const char *name)
    {
#if 0
      return get_type_field_for_class (env, get_java_class (env, name));
#else
      jobject nameString = (*env)->NewStringUTF (env, name);
      jobject method;
      jclass lref, returnType;
      
      if (!(method = (*env)->CallStaticObjectMethod (env,
                                                     c_Primitives,
                                                     m_PrimitivesGetTypeMethod,
                                                     nameString)))
        abort ();
      (*env)->DeleteLocalRef (env, nameString);
      if (!(lref = (*env)->CallObjectMethod (env,
                                             method,
                                             m_MethodGetReturnType,
                                             method)))
        abort ();
      returnType = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);
      return returnType;
#endif
    }
  if (!initFlag)
   {

      c_char = get_primitive ("Character");
      c_byte = get_primitive ("Byte");
      c_int = get_primitive ("Integer");
      c_short = get_primitive ("Short");
      c_long = get_primitive ("Long");
      c_float = get_primitive ("Float");
      c_double = get_primitive ("Double");
      c_void = get_primitive ("Void");
      c_boolean = get_primitive ("Boolean");

      c_Boolean = get_java_class (env, "Boolean");
      c_Char = get_java_class (env, "Character");
      c_Byte= get_java_class (env, "Byte");
      c_Integer = get_java_class (env, "Integer");
      c_Short = get_java_class (env, "Short");
      c_Long = get_java_class (env, "Long");
      c_Float = get_java_class (env, "Float");
      c_Double = get_java_class (env, "Double");
     
      c_String = get_java_class (env, "String");
      c_Object = get_java_class (env, "Object");
      c_Class = get_java_class (env, "Class");

      if (!(lref = (*env)->FindClass (env, "java/lang/reflect/Field")))
        abort ();
      c_Field = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);

      if (!(lref = (*env)->FindClass (env, "swarm/Selector")))
        abort ();
      c_Selector = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);
      
      if (!(lref = (*env)->FindClass (env, "swarm/PhaseCImpl")))
        abort ();
      c_PhaseCImpl = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);

      if (!(lref = (*env)->FindClass (env,
                                      "swarm/SwarmEnvironment")))
	abort ();
      c_SwarmEnvironment = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);

      initFlag = YES;
   }
}

static void 
create_method_refs (JNIEnv *env)
{
  jmethodID findMethodID (const char *name, jclass clazz)
    {
      char sig[31 + strlen (name) + 1 + 1];
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
  
  if (!(m_StringValueOfBoolean = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(Z)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfChar = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(C)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfInt = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(I)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfLong = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(J)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfFloat = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(F)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfDouble = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(D)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfObject = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(Ljava/lang/Object;)Ljava/lang/String;")))
    abort ();

  if (!(m_FieldSet = 
	(*env)->GetMethodID (env, c_Field, "set", 
			     "(Ljava/lang/Object;Ljava/lang/Object;)V")))
    abort();

  if (!(m_FieldSetChar = 
	(*env)->GetMethodID (env, c_Field, "setChar", 
			     "(Ljava/lang/Object;C)V")))
    abort();

  if (!(m_ClassGetDeclaredField =
      (*env)->GetMethodID (env, c_Class, "getDeclaredField",
			   "(Ljava/lang/String;)Ljava/lang/reflect/Field;")))
    abort();

  if (!(m_ClassGetDeclaredFields =
  	(*env)->GetMethodID (env, c_Class, "getDeclaredFields",
			     "()[Ljava/lang/reflect/Field;")))
    abort();
  
  if (!(m_ClassGetDeclaredMethods =
  	(*env)->GetMethodID (env, c_Class, "getDeclaredMethods",
  		     "()[Ljava/lang/reflect/Method;")))
    abort();

  if (!(m_ClassGetName = 
	(*env)->GetMethodID (env, c_Class, "getName", "()Ljava/lang/String;")))
    abort();

  if (!(m_FieldGetName = 
	(*env)->GetMethodID (env, c_Field, "getName", "()Ljava/lang/String;")))
    abort();

  if (!(m_FieldGetType =
	(*env)->GetMethodID (env, c_Field, "getType", "()Ljava/lang/Class;")))
    abort();
  
  if (!(m_FieldGetBoolean =
      (*env)->GetMethodID (env, c_Field, "getBoolean", 
			   "(Ljava/lang/Object;)Z")))
    abort();

  if (!(m_FieldGetChar =
      (*env)->GetMethodID (env, c_Field, "getChar", 
			   "(Ljava/lang/Object;)C")))
    abort();

  if (!(m_FieldGetInt =
      (*env)->GetMethodID (env, c_Field, "getInt", 
			   "(Ljava/lang/Object;)I")))
    abort();

  if (!(m_FieldGetLong =
      (*env)->GetMethodID (env, c_Field, "getLong", 
			   "(Ljava/lang/Object;)J")))
    abort();
  
  if (!(m_FieldGetFloat =
      (*env)->GetMethodID (env, c_Field, "getFloat", 
			   "(Ljava/lang/Object;)F")))
    abort();

  if (!(m_FieldGetDouble =
      (*env)->GetMethodID (env, c_Field, "getDouble", 
			   "(Ljava/lang/Object;)D")))
    abort();

  if (!(m_FieldGetObject =
        (*env)->GetMethodID (env, c_Field, "get",
                             "(Ljava/lang/Object;)Ljava/lang/Object;")))
    abort();
  
  if (!(m_MethodGetName =
	(*env)->GetMethodID (env, c_Method, "getName",
			     "()Ljava/lang/String;")))
    abort();

  if (!(m_SelectorConstructor =
	(*env)->GetMethodID (env, c_Selector, "<init>", 
			     "(Ljava/lang/Class;Ljava/lang/String;Z)V")))
    abort();

  if (!(m_HashCode =
        (*env)->GetMethodID (env, c_Object, "hashCode", "()I")))
    abort ();

  if (!(m_Equals =
        (*env)->GetMethodID (env, c_Object, "equals",
                             "(Ljava/lang/Object;)Z")))
    abort ();
  
  if (!(m_PhaseCImpl_copy_creating_phase_to_using_phase = 
        (*env)->GetMethodID (env,
                             c_PhaseCImpl,
                             "_copy_creating_phase_to_using_phase",
                             "()V")))
    abort();
}


static void
create_field_refs (JNIEnv *env)
{

  if (!(f_nameFid = (*env)->GetFieldID (env, c_Selector, "signature", "Ljava/lang/String;")))
    abort ();
  if (!(f_retTypeFid = (*env)->GetFieldID (env, c_Selector, "retType", "Ljava/lang/Class;")))
    abort ();
  if (!(f_argTypesFid = (*env)->GetFieldID (env, c_Selector, "argTypes", "[Ljava/lang/Class;")))
    abort ();
  if (!(f_typeSignatureFid = (*env)->GetFieldID (env, c_Selector, "typeSignature", "Ljava/lang/String;")))
    abort ();
  if (!(f_objcFlagFid = (*env)->GetFieldID (env, c_Selector, "objcFlag", "Z")))
    abort ();

  if (!(f_nextPhase = (*env)->GetFieldID (env, c_PhaseCImpl, "nextPhase", 
                                          "Ljava/lang/Object;")))
    abort();
}

#if 0
static jstring
get_base_class_name (JNIEnv *env, jobject jobj)
{
  jstring classNameObj =
    swarm_directory_java_class_name (env, jobj);
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


static Class
objc_class_for_class_name (const char *classname)
{
  int len = strlen (classname);
  int end, beg;
  char typename[len + 1];
  
  if (!strcmp ("Impl", classname + len - 4))
    {
      int j = 0;
      if (*(classname + len - 5) == 'C')
        end = len - 5;
      else
        end = len - 4;
      for (beg = 0; (beg < end && j < 2); beg++)
        if (classname[beg] == '.') j++;
      if (j == 2)
        {
          len = end - beg;
          strncpy (typename, &(classname[beg]), len);
          typename[len] = 0;
          return objc_lookup_class (typename);
        }
    }
    return objc_lookup_class (classname);
}

jobject
swarm_directory_java_instantiate (JNIEnv *env, jclass clazz)
{
  jmethodID mid;

  if (!(mid = (*env)->GetMethodID (env, clazz, "<init>", "()V")))
    abort ();
  return (*env)->NewObject (env, clazz, mid);
}

jobject
swarm_directory_next_phase (JNIEnv *env, jobject jobj)
{
  (*env)->CallVoidMethod (env, jobj, 
                          m_PhaseCImpl_copy_creating_phase_to_using_phase);
  return (*env)->GetObjectField (env, jobj, f_nextPhase);
}

static jobject
get_swarmEnvironment_field (JNIEnv *env,
			    jobject swarmEnvironment,
			    const char *fieldName)
{
  jobject fieldObject, ret;
  jstring str = (*env)->NewStringUTF (env, fieldName);

  if (!(fieldObject =
	(*env)->CallObjectMethod (env,
				  c_SwarmEnvironment, 
				  m_ClassGetDeclaredField,
                                  str)))
    abort ();
  (*env)->DeleteLocalRef (env, str);
  
  ret = (*env)->CallObjectMethod (env,
                                  fieldObject,
                                  m_FieldGetObject,
                                  swarmEnvironment);
  (*env)->DeleteLocalRef (env, fieldObject);
  return ret;
}

void
swarm_directory_create_refs (JNIEnv *env)
{
  create_bootstrap_refs (env);
  create_class_refs (env);
  create_method_refs (env);
  create_field_refs (env);
}

void
swarm_directory_init (JNIEnv *env, jobject swarmEnvironment)
{
  void associate (const char *fieldName, id objcObject)
    {
      jobject lref = get_swarmEnvironment_field (env,
                                                 swarmEnvironment,
                                                 fieldName);
      SD_ADD (env,
              lref,
	      objcObject);
      (*env)->DeleteLocalRef (env, lref);
    }
#define ASSOCIATE(fieldName) associate (#fieldName, fieldName)

  jniEnv = env;
  swarmDirectory = [Directory create: globalZone];

  swarm_directory_create_refs (env);

  ASSOCIATE (globalZone);

  ASSOCIATE (hdf5Archiver);
  ASSOCIATE (lispArchiver);
  ASSOCIATE (hdf5AppArchiver);
  ASSOCIATE (lispAppArchiver);
  
  {
    extern id <Symbol> Randomized;
    
    ASSOCIATE (Randomized);
  }

  {
    extern id probeLibrary, probeDisplayManager;
    
    ASSOCIATE (probeLibrary);
    ASSOCIATE (probeDisplayManager);
  }
   
  {
    extern id uniformIntRand, uniformDblRand;
    
    ASSOCIATE (uniformIntRand);
    ASSOCIATE (uniformDblRand);
  }

  {
    extern id <Symbol> ControlStateRunning, ControlStateStopped,
      ControlStateStepping, ControlStateQuit,ControlStateNextTime;
    
    ASSOCIATE (ControlStateRunning);
    ASSOCIATE (ControlStateStopped);
    ASSOCIATE (ControlStateStepping);
    ASSOCIATE (ControlStateQuit);
    ASSOCIATE (ControlStateNextTime);
  }
}

static BOOL
exactclassp (JNIEnv *env, jclass class, jclass matchClass)
{
  return (*env)->IsSameObject (env, class, matchClass);
}

static BOOL
classp (JNIEnv *env, jclass class, jclass matchClass)
{
  jobject clazz;
  
  if ((*env)->IsSameObject (env, class, matchClass))
    return YES;
  else
    {
      jclass nextClass;
      
      for (clazz = (*env)->GetSuperclass (env, class);
           clazz;
           nextClass = (*env)->GetSuperclass (env, clazz), 
             (*env)->DeleteLocalRef (env, clazz),
             clazz = nextClass)
        if ((*env)->IsSameObject (env, clazz, matchClass))
          {
            (*env)->DeleteLocalRef (env, clazz);
            return YES;
          }
      return NO;
    }
}

char
swarm_directory_objc_type_for_java_class (JNIEnv *env, jclass class)
{
  char type;

  if (classp (env, class, c_Selector))
    type = _C_SEL;
  else if (classp (env, class, c_String))
    type = _C_CHARPTR;
  else if (classp (env, class, c_Class))
    type = _C_CLASS;
  else if (exactclassp (env, class, c_int))
    type = _C_INT;
  else if (exactclassp (env, class, c_short))
    type = _C_SHT;
  else if (exactclassp (env, class, c_long))
    type = _C_LNG;
  else if (exactclassp (env, class, c_boolean))
    type = _C_UCHR;
  else if (exactclassp (env, class, c_byte))
    type = _C_UCHR;
  else if (exactclassp (env, class, c_char))
    type = _C_CHR;
  else if (exactclassp (env, class, c_float))
    type = _C_FLT;
  else if (exactclassp (env, class, c_double))
    type = _C_DBL;
  else if (exactclassp (env, class, c_void))
    type = _C_VOID;
  else
    type = _C_ID;
  
  return type;
}

SEL
swarm_directory_ensure_selector (JNIEnv *env, jobject jsel)
{
  SEL sel;

  if (!jsel)
    sel = NULL;
  else if (!(sel = (SEL) SD_FINDOBJC (env, jsel)))
    {
      char *name, *p;
      unsigned i;
      jboolean copyFlag;
      jboolean objcFlag;
      jarray argTypes;
      jsize argCount;
        
      objcFlag = (*env)->GetBooleanField (env, jsel, f_objcFlagFid);
      argTypes = (*env)->GetObjectField (env, jsel, f_argTypesFid);
      argCount = (*env)->GetArrayLength (env, argTypes);

      {
        jstring string;
        const char *utf;
        
        string = (*env)->GetObjectField (env, jsel, f_nameFid);
        utf = (*env)->GetStringUTFChars (env, string, &copyFlag);
        
        if (objcFlag)
          {
            size_t len = strlen (utf);
            BOOL needTrailingColon = argCount > 0 && utf[len - 1] != '$';
            
            p = name = [scratchZone alloc: len + (int) needTrailingColon + 1];
            strcpy (name, utf);
            while (*p)
              {
                if (*p == '$')
                  *p = ':';
                p++;
              }
            if (needTrailingColon)
	    {
              *p++ = ':';
              *p = '\0';
            }
          }
        else
          {
            name = [scratchZone alloc: strlen (utf) + argCount + 1];
            p = stpcpy (name, utf);
            for (i = 0; i < argCount; i++)
              *p++ = ':';
            *p = '\0';
          }
        if (copyFlag)
          (*env)->ReleaseStringUTFChars (env, string, utf);
        (*env)->DeleteLocalRef (env, string);
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
        void add (jclass class)
          {
            add_type (swarm_directory_objc_type_for_java_class (env, class));
          }

        {
          jobject retType = (*env)->GetObjectField (env, jsel, f_retTypeFid);
          
          add (retType);
          (*env)->DeleteLocalRef (env, retType);
        }
        add_type (_C_ID);
        add_type (_C_SEL);

        for (ti = 0; ti < argCount; ti++)
          {
            jobject lref = (*env)->GetObjectArrayElement (env, argTypes, ti);

            add (lref);
            (*env)->DeleteLocalRef (env, lref);
          }
      
        sel = sel_get_any_typed_uid (name);
        if (sel)
          {
            if (!sel_get_typed_uid (name, signatureBuf))
              raiseEvent (WarningMessage,
                          "Method `%s' type (%s) differs from Swarm "
                          "method's type (%s)\n",
                          name, signatureBuf, sel->sel_types);
          }
        else
          {
            const char *type =
              mframe_build_signature (signatureBuf, NULL, NULL, NULL);

            sel = sel_register_typed_name (name, type);
          }
      }
      
      SD_ADD (env, jsel, (id) sel);

      (*env)->DeleteLocalRef (env, argTypes);
      SFREEBLOCK (name);
    }
  return sel;
}

Class
swarm_directory_ensure_class (JNIEnv *env, jclass javaClass)
{
  Class objcClass;

  if (!(objcClass = SD_FINDOBJC (env, javaClass)))
    {
      const char *className = get_class_name (env, javaClass);

      objcClass = objc_class_for_class_name (className);
      FREECLASSNAME (className);
      
      // if the corresponding class does not exist create new Java Proxy
      
      if (objcClass == nil)
        objcClass = [JavaProxy create: globalZone];

      SD_ADD (env, (jobject) javaClass, (id) objcClass);
    }
  return objcClass;
}

const char *
swarm_directory_copy_java_string (JNIEnv *env, jstring javaString)
{
  jboolean isCopy;
  const char *str = (*env)->GetStringUTFChars (env, javaString, &isCopy);
  const char *ret = SSTRDUP (str);

  if (isCopy)
    (*env)->ReleaseStringUTFChars (env, javaString, str);
  return ret;
}

void
swarm_directory_cleanup_strings (JNIEnv *env,
                                 const char **stringArray,
                                 size_t count)
{
  size_t i;

  for (i = 0; i < count; i++)
    SFREEBLOCK (stringArray[i]);
}

#if 0
static Class
swarm_directory_class_from_objc_object (JNIEnv *env, id object)
{
  jobject jobj;

  if ((jobj = SD_FINDJAVA (env, object)))
    {
      jclass jcls;
      Class ret;
      
      jcls = (*env)->GetObjectClass (env,jobj);
      ret = swarm_directory_ensure_class (env, jcls);
      (*env)->DeleteLocalRef (env, jcls);
      return ret;
    }
  else
    return [object getClass];
}
#endif
#endif

Class 
swarm_directory_swarm_class (id object)
{
#ifdef HAVE_JDK
  if (swarmDirectory)
    {
      jobject jobj;
      
      if ((jobj = SD_FINDJAVA (env, object)))
        {
          jclass jcls;
          const char *className;
          Class result;
          
          jcls = (*jniEnv)->GetObjectClass (jniEnv, jobj);
          className = swarm_directory_java_class_name (jniEnv, jobj);
          result = objc_class_for_class_name (className);
          FREECLASSNAME (className);
          if (!result)
            if (!(result = SD_FINDOBJC (jniEnv, jcls)))
              result = swarm_directory_ensure_class (jniEnv, jcls);
          (*jniEnv)->DeleteLocalRef (jniEnv, jcls);
          return result;
        }
    }
#endif
  return [object getClass];
}

const char *
swarm_directory_language_independent_class_name  (id object)
{
#ifdef HAVE_JDK
  if (swarmDirectory)
    {
      jobject jobj;
      
      if ((jobj = SD_FINDJAVA (jniEnv, object)))
        return swarm_directory_java_class_name (jniEnv, jobj);
    }
  return (const char *) (getClass (object))->name;      
#else
  return (const char *) (getClass (object))->name;
#endif
}

#ifdef HAVE_JDK
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

const char *
swarm_directory_signature_for_class (JNIEnv *env, jclass class)
{
  const char *type;

  if (classp (env, class, c_Selector))
    type = "Lswarm/Selector;";
  else if (classp (env, class, c_String))
    type = "Ljava/lang/String;";
  else if (classp (env, class, c_Class))
    type = "Ljava/lang/Class;";
  else if (exactclassp (env, class, c_int))
    type = "I";
  else if (exactclassp (env, class, c_short))
    type = "S";
  else if (exactclassp (env, class, c_long))
    type = "L";
  else if (exactclassp (env, class, c_boolean))
    type = "Z";
  else if (exactclassp (env, class, c_byte))
    type = "B";
  else if (exactclassp (env, class, c_char))
    type = "C";
  else if (exactclassp (env, class, c_float))
    type = "F";
  else if (exactclassp (env, class, c_double))
    type = "D";
  else if (exactclassp (env, class, c_void))
    type = "V";
  else
    {
      const char *name = get_class_name (env, class);
      char *buf = [scratchZone alloc: 1 + strlen (name) + 1 + 1];

      fill_signature (buf, name);
      return buf;
    }
  return SSTRDUP (type);
}

const char *
swarm_directory_ensure_selector_type_signature (JNIEnv *env, jobject jsel)
{
  unsigned argCount, typeSigLen = 0;

  jobject typeSignature =
    (*env)->GetObjectField (env, jsel, f_typeSignatureFid);

  if (!typeSignature)
    {
      jobject argTypes = 
        (*env)->GetObjectField (env, jsel, f_argTypesFid);

      argCount = (*env)->GetArrayLength (env, argTypes);
      {
        const char *argSigs[argCount];
        const char *retSig;
        char *sig, *p;
        unsigned ai;
        
        typeSigLen = 1;
        for (ai = 0; ai < argCount; ai++)
          {
            jclass member =
              (*env)->GetObjectArrayElement (env, argTypes, ai);
            
            argSigs[ai] = swarm_directory_signature_for_class (env, member);
            typeSigLen += strlen (argSigs[ai]);
            (*env)->DeleteLocalRef (env, member);
          }
        typeSigLen++;

        {
          jobject retType =
            (*env)->GetObjectField (env, jsel, f_retTypeFid);
          
          retSig = swarm_directory_signature_for_class (env, retType);
          (*env)->DeleteLocalRef (env, retType);
        }
        typeSigLen += strlen (retSig);
        
        sig = [scratchZone alloc: typeSigLen + 1];
        
        p = sig;
        *p++ = '(';
        for (ai = 0; ai < argCount; ai++)
          p = stpcpy (p, argSigs[ai]);
        *p++ = ')';
        p = stpcpy (p, retSig);

        for (ai = 0; ai < argCount; ai++)
          [scratchZone free: (void *) argSigs[ai]];
	[scratchZone free: (void *) retSig];

        {
          jobject str = (*env)->NewStringUTF (env, sig);
          (*env)->SetObjectField (env,
                                  jsel,
                                  f_typeSignatureFid,
                                  str);
          (*env)->DeleteLocalRef (env, str);
        }
        (*env)->DeleteLocalRef (env, argTypes);
        return sig;
      }
    }
  else
    {
      jboolean copyFlag;
      const char *sig;

      const char *utf =
        (*env)->GetStringUTFChars (env, typeSignature, &copyFlag);

      sig = SSTRDUP (utf);
      if (copyFlag)
        (*env)->ReleaseStringUTFChars (env, typeSignature, utf);
      (*env)->DeleteLocalRef (env, typeSignature);
      return sig;
    }
}
#endif
