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

#import <defobj.h>

#import <collections.h> // Map

#define extern
#ifdef HAVE_JDK
#import "java.h" // java_find_class
#endif
#undef extern

#define internalimplementation implementation // defeat make-h2x

#ifdef HAVE_JDK

extern JNIEnv *jniEnv;

Directory *swarmDirectory;

static const char *
getObjcName (JNIEnv *env, DirectoryEntry *entry)
{
  if ((entry->type == foreign_java
       && java_selector_p (env, entry->foreignObject.java))
#if 0
      ||
      (entry->type == foreign_COM
       && COM_selector_p (env, entry->foreignObject.COM))
#endif
      )
    return entry->object ? sel_get_name ((SEL) entry->object) : "M(<nil>)";
  else
    return entry->object ? [entry->object name] : "nil";
}

@internalimplementation DirectoryEntry
- setCOMObject: (void *)theCOMObject
{
  type = foreign_COM;
  foreignObject.COM = theCOMObject;
  return self;
}

- setJavaObject: (jobject)theJavaObject
{
  type = foreign_java;
  foreignObject.java = theJavaObject;
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
  if (entry->type == foreign_java)
    (*env)->DeleteGlobalRef (env, entry->foreignObject.java);
  [getZone (entry) freeIVars: entry];
}

- (void)describe: outputCharStream
{
  [outputCharStream catPointer: self];
  [outputCharStream catC: " objc: "];
  [outputCharStream catC: getObjcName (jniEnv, self)];
  [outputCharStream catC: " "];
  [outputCharStream catPointer: object];

  if (type == foreign_java)
    {
      const char *className =
        swarm_directory_java_class_name (jniEnv, foreignObject.java);
      
      [outputCharStream catC: "  java: "];
      [outputCharStream catC: className];
      [outputCharStream catC: " "];
      [outputCharStream catPointer: foreignObject.java];
      [outputCharStream catC: "\n"];
      FREECLASSNAME (className);
    }
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

@internalimplementation Directory
+ createBegin: aZone
{
  Directory *obj = [super createBegin: aZone];
  size_t size = sizeof (id) * DIRECTORY_SIZE;

  obj->table = [aZone alloc: size];
  memset (obj->table, 0, size);
  obj->objc_tree = avl_create (compare_objc_objects, NULL);
  return obj;
}

DirectoryEntry *
swarm_directory_objc_find (JNIEnv *env, id object)
{
  if (object)
    {
      DirectoryEntry *ret;
      
      ret = avl_find (swarmDirectory->objc_tree, OBJC_FINDENTRY (object));
      return ret;
    }
  return nil;
}

BOOL
swarm_directory_objc_remove (JNIEnv *env, id object)
{
  DirectoryEntry *entry = swarm_directory_objc_find (env, object);

  if (entry)
    {
      if (entry->type == foreign_java)
        {
          unsigned index;
          id <Map> m;
          
          index = swarm_directory_java_hash_code (env, entry->foreignObject.java);
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


Class
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

void
swarm_directory_init (JNIEnv *env, jobject swarmEnvironment)
{
  jniEnv = env;

  swarmDirectory = [Directory create: globalZone];
  
  java_associate_objects (env, swarmEnvironment);
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
      
      if ((jobj = SD_JAVA_FINDJAVA (env, object)))
        {
          jclass jcls;
          const char *className;
          Class result;
          
          jcls = (*env)->GetObjectClass (env, jobj);
          className = swarm_directory_java_class_name (env, jobj);
          result = objc_class_for_class_name (className);
          FREECLASSNAME (className);
          if (!result)
            if (!(result = SD_JAVA_FINDOBJC (env, jcls)))
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
      
      if ((jobj = SD_JAVA_FINDJAVA (env, object)))
        return swarm_directory_java_class_name (env, jobj);
    }
  return (const char *) (getClass (object))->name;      
#else
  return (const char *) (getClass (object))->name;
#endif
}


