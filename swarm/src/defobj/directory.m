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

#include <swarmconfig.h>  // HAVE_JDK

#ifdef HAVE_JDK
#import "java.h" // swarm_directory_{java_hash_code,find_class_named,class_for_object}, SD_JAVA_FINDJAVA
#endif

Directory *swarmDirectory;

#define internalimplementation implementation // defeat make-h2x


@internalimplementation DirectoryEntry
- setCOMObject: (void *)theCOMObject
{
  type = foreign_COM;
  foreignObject.COM = theCOMObject;
  return self;
}

- setJavaObject: (JOBJECT)theJavaObject
{
  type = foreign_java;
  foreignObject.java = theJavaObject;
  return self;
}

void
swarm_directory_entry_drop (DirectoryEntry *entry)
{
#ifdef HAVE_JDK
  if (entry->type == foreign_java)
    java_drop (entry->foreignObject.java);
#endif
  [getZone (entry) freeIVars: entry];
}

- (void)describe: outputCharStream
{
#ifdef HAVE_JDK
  if (type == foreign_java)
    {
      const char *className =
        java_class_name (foreignObject.java);
      
      [outputCharStream catC: "  java: "];
      [outputCharStream catC: className];
      [outputCharStream catC: " "];
      [outputCharStream catPointer: foreignObject.java];
      [outputCharStream catC: "\n"];
      FREECLASSNAME (className);
    }
#endif
}

@end

@implementation ObjectEntry
- setObject: theObject
{
  object = theObject;
  return self;
}

- (void)describe: stream
{
  [stream catPointer: self];
  [stream catC: " object: "];
  [stream catC: object ? [object name] : "nil"];
  [stream catC: " "];
  [stream catPointer: object];
  [super describe: stream];
}
@end

@implementation SelectorEntry
- setSelector: (SEL)theSelector;
{
  selector = theSelector;
  return self;
}

- (void)describe: stream
{
  [stream catPointer: self];
  [stream catC: " selector: "];
  [stream catC: selector ? sel_get_name (selector) : "M(<nil>)"];
  [super describe: stream];
}
@end

static int
compare_objc_selectors (const void *A, const void *B, void *PARAM)
{
  SelectorEntry *a = (SelectorEntry *) A;
  SelectorEntry *b = (SelectorEntry *) B;

  const char *aname = sel_get_name (a->selector);
  const char *bname = sel_get_name (b->selector);
  
  return strcmp (aname, bname);
}

static int
compare_objc_objects (const void *A, const void *B, void *PARAM)
{
  ObjectEntry *a = (ObjectEntry *) A;
  ObjectEntry *b = (ObjectEntry *) B;

  if (a->object < b->object)
    return -1;
  
  return a->object > b->object;
}

@internalimplementation Directory
+ createBegin: aZone
{
  Directory *obj = [super createBegin: aZone];
  size_t size = sizeof (id) * DIRECTORY_SIZE;

  obj->table = [aZone alloc: size];
  memset (obj->table, 0, size);
  obj->object_tree = avl_create (compare_objc_objects, NULL);
  obj->selector_tree = avl_create (compare_objc_selectors, NULL);
  return obj;
}

ObjectEntry *
swarm_directory_objc_find_object (id object)
{
  if (object)
    {
      ObjectEntry *ret;
      
      ret = avl_find (swarmDirectory->object_tree,
                      OBJC_FIND_OBJECT_ENTRY (object));
      return ret;
    }
  return nil;
}

SelectorEntry *
swarm_directory_objc_find_selector (SEL sel)
{
  SelectorEntry *ret;
  
  ret = avl_find (swarmDirectory->selector_tree,
                  OBJC_FIND_SELECTOR_ENTRY (sel));
  return ret;
}

BOOL
swarm_directory_objc_remove (id object)
{
  ObjectEntry *entry = swarm_directory_objc_find_object (object);

  if (entry)
    {
#ifdef HAVE_JDK
      if (entry->type == foreign_java)
        {
          unsigned index;
          id <Map> m;
          
          index = swarm_directory_java_hash_code (entry->foreignObject.java);
          m = swarmDirectory->table[index];
          if (!m)
            abort ();
          {
            ObjectEntry *ret;
            
            ret = [m remove: entry];
            
            if (ret != entry)
              raiseEvent (WarningMessage, "remove (%p) != %p\n", entry, ret);
            
            ret = avl_delete (swarmDirectory->object_tree, entry);
            
            if (ret != entry)
              abort ();
          }
          swarm_directory_entry_drop (entry);
          return YES;
        }
#endif
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
swarm_directory_dump (void)
{
  xprint (swarmDirectory);
}

Class
swarm_directory_ensure_class_named (const char *className)
{
  Class objcClass = Nil;
  if (swarmDirectory)
    {
#ifdef HAVE_JDK
      objcClass = swarm_directory_java_find_class_named (className);
#endif
    }
  if (!objcClass)
    objcClass = objc_lookup_class (className);
  return objcClass;
}

Class
swarm_directory_swarm_class (id object)
{
  if (swarmDirectory)
    {
      ObjectEntry *entry = swarm_directory_objc_find_object (object);

      if (!entry)
        abort ();

#ifdef HAVE_JDK
      if (entry->type == foreign_java)
        {
          jobject jobj;

          if ((jobj = SD_JAVA_FIND_OBJECT_JAVA (object)))
            return swarm_directory_java_class_for_object (jobj);
        }
      else
#endif
        abort ();
    }
  return [object getClass];
}

Class
swarm_directory_superclass (Class class)
{
  if ([class respondsTo: M(isJavaProxy)])
    {
      jclass clazz = SD_JAVA_FIND_OBJECT_JAVA (class);
      
      return SD_JAVA_ENSUREOBJCCLASS ((*jniEnv)->GetSuperclass (jniEnv,
								clazz));
    }
  else
    return class_get_super_class (class);
}


const char *
swarm_directory_language_independent_class_name  (id object)
{
  if (swarmDirectory)
    {
      ObjectEntry *entry = swarm_directory_objc_find_object (object);

      if (entry)
        {
#ifdef HAVE_JDK
          if (entry->type == foreign_java)
            {
              jobject jobj;
              
              if ((jobj = SD_JAVA_FIND_OBJECT_JAVA (object)))
                return java_class_name (jobj);
            }
          else
#endif
            abort ();
        }
    }
  return (const char *) (getClass (object))->name;      
}


