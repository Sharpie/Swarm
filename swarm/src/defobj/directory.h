// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Include before jni.h to avoid jmalloc screwing up stdlib malloc decls.
#include <misc.h> 
#include <swarmconfig.h>
#ifdef JNI_H_NEEDS_INT64
#define __int64 INT64
#endif
#ifdef __osf__
#define _REENTRANT
#endif

#include <objc/objc.h>
Class swarm_directory_swarm_class (id object);
const char *swarm_directory_language_independent_class_name (id object);
Class swarm_directory_ensure_class_named (const char *className);

#ifdef HAVE_JDK
#undef SIZEOF_LONG /* Kaffe can define this (it's from swarmconfig.h) */
#include <jni.h>
#include <misc/avl.h>

#import <defobj/Create.h>

extern JNIEnv *jniEnv;

void swarm_directory_init (JNIEnv *jniEnv,
                          jobject swarmEnvironment);

typedef enum { foreign_java, foreign_COM } foreign_t;

@interface DirectoryEntry: CreateDrop
{
@public
  foreign_t type;
  id object;
  union {
    jobject java;
    void *COM;
  } foreignObject;
}
- setObject: object;
- setJavaObject: (jobject)javaObject;
- setCOMObject: (void *)COMObject;

void swarm_directory_entry_drop (JNIEnv *env, DirectoryEntry *entry);
- (void)describe: outputCharStream;
@end

@interface Directory: CreateDrop
{
@public
  id *table;
  avl_tree *objc_tree;
}
+ createBegin: aZone;
DirectoryEntry *swarm_directory_objc_find (JNIEnv *env, id object);
BOOL swarm_directory_objc_remove (JNIEnv *env, id obj);
- (void)describe: outputCharStream;
@end

extern Directory *swarmDirectory;

extern void swarm_directory_dump ();

#import "java.h"

#define SD_JAVA_FINDOBJC(env, jobj)  swarm_directory_java_find_objc (env, jobj)
#define SD_JAVA_ENSUREOBJC(env, jobj) swarm_directory_java_ensure_objc (env, jobj)
#define SD_JAVA_FINDJAVA(env, objc) swarm_directory_objc_find_java (env, objc)
#define SD_JAVA_ENSUREJAVA(env, objc) swarm_directory_objc_ensure_java (env, objc)
#define SD_JAVA_FINDJAVACLASS(env, objcClass) swarm_directory_objc_find_java_class (env, objcClass)
#define SD_JAVA_ADD(env, jobj, objc) swarm_directory_java_add (env, objc, jobj)
#define SD_JAVA_ADDJAVA(env, jobj, objc) swarm_directory_java_add (env, objc, jobj)->foreignObject.java
#define SD_JAVA_NEXTPHASE(env, jobj, objc) swarm_directory_java_switch_phase (env, objc, jobj)->foreignObject.java
#define SD_JAVA_SWITCHOBJC(env, jobj, newobjc) swarm_directory_java_switch_objc (env, newobjc, jobj)
#define SD_JAVA_INSTANTIATE(env, clazz) swarm_directory_java_instantiate (env, clazz)
#define SD_JAVA_NEXTJAVAPHASE(env, jobj) swarm_directory_java_next_phase (env, jobj)
#define SD_JAVA_ENSUREOBJCMETHOD(env, jobj) swarm_directory_java_ensure_selector (env, jobj)
#define SD_JAVA_ENSUREOBJCCLASS(env, jclass) swarm_directory_java_ensure_class (env, jclass)

void swarm_directory_cleanup_strings (JNIEnv *env, const char **stringArray, size_t count);

#define SD_JAVA_COPYSTRING(env, javaString) swarm_directory_java_copy_string (env, javaString)
#define SD_JAVA_CLEANUPSTRINGS(env, stringArray) swarm_directory_cleanup_strings (env, stringArray, sizeof (stringArray) / sizeof (const char *))
#endif

#define SD_GETCLASS(obj) swarm_directory_swarm_class (obj)

#if 1
#define DIRECTORY_SIZE 21599
#else
#define DIRECTORY_SIZE 263009
#endif

extern void *alloca (size_t);

#define JAVA_ENTRY(theObject,theJavaObject) [[[[DirectoryEntry createBegin: globalZone] setJavaObject: theJavaObject] setObject: theObject] createEnd]
#define JAVA_OBJCENTRY(theObject) JAVA_ENTRY(theObject,0)
#define JAVA_JAVAENTRY(theJavaObject) JAVA_ENTRY(0,theJavaObject)
#define JAVA_FINDENTRY(theJavaObject) ({ DirectoryEntry *_findEntry  = alloca (sizeof (DirectoryEntry)); _findEntry->foreignObject.java = theJavaObject; _findEntry; })

#define OBJC_FINDENTRY(theObject) ({ DirectoryEntry *_findEntry  = alloca (sizeof (DirectoryEntry)); _findEntry->object = theObject; _findEntry; })

Class objc_class_for_class_name (const char *classname);

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

