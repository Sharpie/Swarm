// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/directory.h>

#import <defobj/DefObject.h>
#import <defobj/defalloc.h>

#include <misc.h>
#include <misc/avl.h>
#include <objc/objc.h>
#include <objc/objc-api.h>
#include <objc/mframe.h> // mframe_build_signature

#import <defobj/JavaProxy.h>
#import <defobj/JavaCollection.h>
#import <defobj.h>

#import <defobj/Program.h> // Type_c
#import <collections.h> // Map

#import "internal.h" // objc_type_for_fcall_type

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

Directory *swarmDirectory;

#if 1
#define DIRECTORY_SIZE 21599
#else
#define DIRECTORY_SIZE 263009
#endif

static const char *
getObjcName (JNIEnv *env, jobject javaObject, id object)
{
  if ((*env)->IsInstanceOf (env, javaObject, c_Selector))
    return object ? sel_get_name ((SEL) object) : "M(<nil>)";
  else
    return object ? [object name] : "nil";
}

const char *
swarm_directory_java_class_name (JNIEnv *env, jobject obj)
{
  jclass class;
  const char *ret;

  if (!(class = (*env)->GetObjectClass (env, obj)))
    abort ();

  ret = java_get_class_name (env, class);
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

void
swarm_directory_entry_drop (JNIEnv *env, DirectoryEntry *entry)
{
  (*env)->DeleteGlobalRef (env, entry->javaObject);
  [getZone (entry) freeIVars: entry];
}

void
swarm_directory_entry_describe (JNIEnv *env,
                                DirectoryEntry *entry,
                                id outputCharStream)
{
  const char *className =
    swarm_directory_java_class_name (env, entry->javaObject);
  
  [outputCharStream catPointer: entry];
  [outputCharStream catC: " objc: "];
  [outputCharStream catC: getObjcName (env, entry->javaObject, entry->object)];
  [outputCharStream catC: " "];
  [outputCharStream catPointer: entry->object];
  [outputCharStream catC: "  java: "];
  [outputCharStream catC: className];
  [outputCharStream catC: " "];
  [outputCharStream catPointer: entry->javaObject];
  [outputCharStream catC: "\n"];
  FREECLASSNAME (className);
}

- (void)describe: outputCharStream
{
  swarm_directory_entry_describe (jniEnv, self, outputCharStream);  
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

static DirectoryEntry *
swarm_directory_java_find (JNIEnv *env, jobject javaObject)
{
  if (javaObject)
    {
      id ret;
      unsigned index = swarm_directory_java_hash_code (env, javaObject);
      id <Map> m = swarmDirectory->table[index];
      DirectoryEntry *findEntry = swarmDirectory->findEntry;
      
      findEntry->javaObject = javaObject;
      ret = m ? [m at: findEntry] : nil;
      return ret;
    }
  return nil;
}

id
swarm_directory_java_find_objc (JNIEnv *env, jobject javaObject)
{
  DirectoryEntry *entry = swarm_directory_java_find (env, javaObject);

  return entry ? entry->object : nil;
}

static DirectoryEntry *
swarm_directory_objc_find (JNIEnv *env, id object)
{
  if (object)
    {
      DirectoryEntry *ret;
      
      ret = avl_find (swarmDirectory->objc_tree, OBJCFINDENTRY (object));
      return ret;
    }
  return nil;
}

jobject
swarm_directory_objc_find_java (JNIEnv *env, id object)
{
  DirectoryEntry *entry = swarm_directory_objc_find (env, object);

  return entry ? entry->javaObject : NULL;
}

static int
compareDirectoryEntries (DirectoryEntry *obj1, DirectoryEntry* obj2)
{
#if 0
  printf ("`%s'%p/%p vs %p\n",
          getObjcName (env, obj1->javaObject, obj2->object),
          object, javaObject,
          ((DirectoryEntry *) obj2)->javaObject);
#endif
  return (int) !(*jniEnv)->CallBooleanMethod (jniEnv,
                                              obj1->javaObject,
                                              m_Equals,
                                              obj2->javaObject);
}

static id <Map>
createDirectoryEntryMap (JNIEnv *env)
{
  return [[[Map createBegin: getZone (swarmDirectory)]
            setCompareFunction: compareDirectoryEntries]
           createEnd];
}

DirectoryEntry *
swarm_directory_add (JNIEnv *env, id object, jobject lref)
{
  unsigned index;
  id <Map> m;
  DirectoryEntry *entry;
  jobject javaObject = (*env)->NewGlobalRef (env, lref);
  id *table = swarmDirectory->table;
  
  entry = ENTRY (object, javaObject);
  index = swarm_directory_java_hash_code (env, javaObject);
  m = table[index];

  if (m == nil)
    {
      m = createDirectoryEntryMap (env);
      table[index] = m;
    }
  [m at: entry insert: entry];
  avl_probe (swarmDirectory->objc_tree, entry);
  return entry;
}

static void
swarm_directory_switch_java_entry (JNIEnv *env, 
                                   DirectoryEntry *entry, jobject javaObject)
{
  unsigned index;
  id <Map> m;
  id <Map> *table = swarmDirectory->table;
  
  javaObject = (*env)->NewGlobalRef (env, javaObject);
  index = swarm_directory_java_hash_code (env, entry->javaObject);
  m = table[index];
  [m remove: entry];
  (*env)->DeleteGlobalRef (env, entry->javaObject);
  
  index = swarm_directory_java_hash_code (env, javaObject);
  entry->javaObject = javaObject;
  if (!table[index])
    table[index] = createDirectoryEntryMap (env);

  [table[index] at: entry insert: entry];
}

#if 0
DirectoryEntry *
swarm_directory_switch_java (JNIEnv *env,
                             id object,
                             jobject javaObject)
{
  DirectoryEntry *entry;
  
  if (!(entry = avl_find (swarmDirectory->objc_tree, OBJCFINDENTRY (object))))
    abort ();
  swarm_directory_switch_java_entry (env, entry, javaObject);
  return entry;
}
#endif

DirectoryEntry *
swarm_directory_switch_phase (JNIEnv *env,
                              id nextPhase,
                              jobject currentJavaPhase)
{
  jobject nextJavaPhase = SD_NEXTJAVAPHASE (env, currentJavaPhase);
  id currentPhase = SD_FINDOBJC (env, currentJavaPhase);
  DirectoryEntry *retEntry;
  avl_tree *objc_tree = swarmDirectory->objc_tree;
  
  if (currentPhase != nextPhase)
    {
      id entry = ENTRY (nextPhase, currentJavaPhase);
      DirectoryEntry **entryptr = 
        (DirectoryEntry **) avl_probe (objc_tree, entry);

      if (*entryptr != entry)
        abort ();

      swarm_directory_switch_java_entry (env, entry, nextJavaPhase);
      {
        id ret;
        
        ret = avl_delete (objc_tree, OBJCFINDENTRY (currentPhase));
        if (!ret)
          abort ();
        swarm_directory_entry_drop (env, ret);
      }
      retEntry = *entryptr;
    }
  else
    {
      DirectoryEntry *entry = avl_find (objc_tree, OBJCFINDENTRY (nextPhase));
      
      if (!entry)
        abort ();

      swarm_directory_switch_java_entry (env, entry, nextJavaPhase);
      
      retEntry = entry;
    }
  (*env)->DeleteLocalRef (env, nextJavaPhase);
  return retEntry;
}

DirectoryEntry *
swarm_directory_switch_objc (JNIEnv *env,
                             id object,
                             jobject javaObject)
{
  DirectoryEntry *entry;
  unsigned index;
  id <Map> m;
  id *table = swarmDirectory->table;
  avl_tree *objc_tree = swarmDirectory->objc_tree;
  
  index = swarm_directory_java_hash_code (env, javaObject);
  m = table[index];
  entry = [table[index] at: JAVAFINDENTRY (javaObject)];
  if (!entry)
    abort ();
  
  if (!avl_delete (objc_tree, entry))
    abort ();
  entry->object = object;

  {
    void **foundEntry;
    
    foundEntry = avl_probe (objc_tree, entry);
    if (*foundEntry != entry)
      abort ();
  }
  return entry;
}

id
swarm_directory_java_ensure_objc (JNIEnv *env, jobject javaObject)
{
  if (!javaObject)
    return nil;
  else
    {
      DirectoryEntry *result; 
      
      if (!javaObject)
        return nil;
  
      result = swarm_directory_java_find (env, javaObject);
      
      if ((*env)->IsInstanceOf (env, javaObject, c_String))
        {
          jboolean isCopy;
          const char *utf, *str;
          
          utf = (*env)->GetStringUTFChars (env, javaObject, &isCopy);
          str = ZSTRDUP (getZone (swarmDirectory), utf);
          if (isCopy)
            (*env)->ReleaseStringUTFChars (env, javaObject, utf);
          
          if (result)
            {
              const char *last = (const char *) result->object;

              result = SD_SWITCHOBJC (env, javaObject, (id) str);
	      ZFREEBLOCK (getZone (swarmDirectory), (void *) last);
            }
          else
            result = SD_ADD (env, javaObject, (id) str);
        }
      else if (!result)
        result =
          SD_ADD (env, javaObject, 
                  ((*env)->IsInstanceOf (env, javaObject, c_Collection)
                   ? [JavaCollection create: globalZone]
                   : [JavaProxy create: globalZone]));

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
objcFindJavaClassName (JNIEnv *env, Class class)
{
  const char *javaClassName;
  
  if (getBit (class->info, _CLS_DEFINEDCLASS))
    {
      Type_c *typeImpl;
      Class_s *nextPhase;

      nextPhase = ((BehaviorPhase_s *) class)->nextPhase;
      typeImpl = [class getTypeImplemented];
      javaClassName = java_classname_for_typename (env,
						   typeImpl->name,
						   nextPhase == NULL); 
    }
  else
    {
      Type_c *typeImpl;
      typeImpl = [class getTypeImplemented];

      if (typeImpl)
        javaClassName =
	  java_classname_for_typename (env, typeImpl->name, YES);
      else 
        javaClassName =
	  java_classname_for_typename (env, class->name, YES);
    }
  return javaClassName;
}

jclass
swarm_directory_objc_find_java_class (JNIEnv *env, Class class)
{
  jclass ret;
  const char *javaClassName = objcFindJavaClassName (env, class);

  ret = java_find_class (env, javaClassName, YES);
  FREECLASSNAME (javaClassName);
  return ret;
}

BOOL
swarm_directory_objc_remove (JNIEnv *env, id object)
{
  DirectoryEntry *entry = swarm_directory_objc_find (env, object);

  if (entry)
    {
      unsigned index;
      id <Map> m;

      index = swarm_directory_java_hash_code (env, entry->javaObject);
      m = swarmDirectory->table[index];
      if (!m)
        abort ();
      {
        DirectoryEntry *ret;

        ret = [m remove: entry];

        if (ret != entry)
          raiseEvent (WarningMessage, "remove (%p) != %p\n", entry, ret);

        ret = avl_delete (swarmDirectory->objc_tree, entry);
        
        if (ret != entry)
          abort ();
      }
      swarm_directory_entry_drop (env, entry);
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
get_java_lang_class (JNIEnv *env, const char *name)
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
      return get_type_field_for_class (env, get_java_lang_class (env, name));
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

      c_Boolean = get_java_lang_class (env, "Boolean");
      c_Char = get_java_lang_class (env, "Character");
      c_Byte= get_java_lang_class (env, "Byte");
      c_Integer = get_java_lang_class (env, "Integer");
      c_Short = get_java_lang_class (env, "Short");
      c_Long = get_java_lang_class (env, "Long");
      c_Float = get_java_lang_class (env, "Float");
      c_Double = get_java_lang_class (env, "Double");
     
      c_String = get_java_lang_class (env, "String");
      c_Object = get_java_lang_class (env, "Object");
      c_Class = get_java_lang_class (env, "Class");

      if (!(lref = (*env)->FindClass (env, "java/lang/reflect/Field")))
        abort ();
      c_Field = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);

      if (!(lref = (*env)->FindClass (env, "java/util/Collection")))
        abort ();
      c_Collection = (*env)->NewGlobalRef (env, lref);
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

  if (!(m_ClassGetFields =
  	(*env)->GetMethodID (env, c_Class, "getFields",
			     "()[Ljava/lang/reflect/Field;")))
    abort();
  
  if (!(m_ClassGetName = 
	(*env)->GetMethodID (env, c_Class, "getName", "()Ljava/lang/String;")))
    abort();

  if (!(m_ClassIsArray =
	(*env)->GetMethodID (env, c_Class, "isArray", "()Z")))
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

  if (!(m_FieldGetShort =
      (*env)->GetMethodID (env, c_Field, "getShort", 
			   "(Ljava/lang/Object;)S")))
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
    extern id <Symbol> Sequential;
    
    ASSOCIATE (Randomized);
    ASSOCIATE (Sequential);
  }

  {
    extern id probeLibrary, probeDisplayManager;
    
    ASSOCIATE (probeLibrary);
    ASSOCIATE (probeDisplayManager);
  }
   
  {
    extern id randomGenerator, uniformIntRand, uniformDblRand;
    
    ASSOCIATE (randomGenerator);
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

jobject
swarm_directory_objc_ensure_java (JNIEnv *env, id object)
{
  DirectoryEntry *result; 

  if (!object)
    return 0;

  result = swarm_directory_objc_find (env, object);
  
  if (!result)
    {
      Class class = getClass (object);
      jclass javaClass = swarm_directory_objc_find_java_class (env, class);
      jobject lref = swarm_directory_java_instantiate (env, javaClass);
      
      result = SD_ADD (env, lref, object);
      (*env)->DeleteLocalRef (env, lref);
      (*env)->DeleteLocalRef (env, javaClass);
    }
  return result->javaObject;
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
            const char *type = 
              objc_type_for_fcall_type (fcall_type_for_java_class (env, class));
            add_type (*type);
            [globalZone free: (void *) type];
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
swarm_directory_java_ensure_class (JNIEnv *env, jclass javaClass)
{
  Class objcClass;

  if (!(objcClass = SD_FINDOBJC (env, javaClass)))
    {
      const char *className = java_get_class_name (env, javaClass);

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

void
swarm_directory_dump (void)
{
  xprint (swarmDirectory);
}
#endif

Class
swarm_directory_ensure_class_named (const char *className)
{
  Class objcClass = nil;
#ifdef HAVE_JDK
  if (swarmDirectory)
    {
      jclass javaClass = java_find_class (jniEnv, className, NO);
      
      if (javaClass)
        {
          objcClass = swarm_directory_java_ensure_class (jniEnv, javaClass);
          (*jniEnv)->DeleteLocalRef (jniEnv, javaClass);
        }
    }
#endif
  if (!objcClass)
    objcClass = objc_lookup_class (className);
  return objcClass;
}

Class 
swarm_directory_swarm_class (id object)
{
#ifdef HAVE_JDK
  if (swarmDirectory)
    {
      jobject jobj;
      JNIEnv *env = jniEnv;
      
      if ((jobj = SD_FINDJAVA (env, object)))
        {
          jclass jcls;
          const char *className;
          Class result;
          
          jcls = (*env)->GetObjectClass (env, jobj);
          className = swarm_directory_java_class_name (env, jobj);
          result = objc_class_for_class_name (className);
          FREECLASSNAME (className);
          if (!result)
            if (!(result = SD_FINDOBJC (env, jcls)))
              result = swarm_directory_java_ensure_class (env, jcls);
          (*env)->DeleteLocalRef (env, jcls);
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
      JNIEnv *env = jniEnv;
      jobject jobj;
      
      if ((jobj = SD_FINDJAVA (env, object)))
        return swarm_directory_java_class_name (env, jobj);
    }
  return (const char *) (getClass (object))->name;      
#else
  return (const char *) (getClass (object))->name;
#endif
}

