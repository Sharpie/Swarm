// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <defobj/directory.h>
#import <defobj/Program.h> // Type_c

#import <defobj/DefObject.h>
#import <defobj/defalloc.h>

#include <misc.h>
#include <misc/avl.h>
#import <defobj/swarm-objc-api.h>

#import <defobj.h>

#import <collections.h> // Map

#include <swarmconfig.h>  // HAVE_JDK

#ifdef HAVE_JDK
#import "java.h" // swarm_directory_java_{hash_code,find_class_named}, SD_JAVA_FIND_OBJECT_JAVA
#endif
#import "COM.h" // SD_COM_FIND_OBJECT_COM

Directory *swarmDirectory = nil;

@implementation DirectoryEntry
- setCOMObject: (COMOBJECT)theCOMObject
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
  if (type == foreign_COM)
    {
      const char *className = COM_class_name (foreignObject.COM);
      
      [outputCharStream catC: "  COM: "];
      [outputCharStream catC: className];
      [outputCharStream catC: " "];
      [outputCharStream catPointer: foreignObject.COM];
      [outputCharStream catC: "\n"];
      FREECLASSNAME (className);
    }
#ifdef HAVE_JDK
  else if (type == foreign_java)
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
  if (object)
    {
      if ([object isInstance])
        {
          [stream catC: " object: "];
          [stream catC: [object name]];
        }
      else
        {
          [stream catC: " class: "];
#if SWARM_OBJC_DONE
	  [stream catC: ((Class) object)->name];
#else
          [stream catC: swarm_class_getName((id)object)];
#endif
        }
    }
  else
    [stream catC: "nil"];
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
  [stream catC: selector ? swarm_sel_getName (selector) : "M(<nil>)"];
  [super describe: stream];
}
@end

static int
compare_objc_selectors (const void *A, const void *B, void *PARAM)
{
  SelectorEntry *a = (SelectorEntry *) A;
  SelectorEntry *b = (SelectorEntry *) B;

  const char *aname = swarm_sel_getName (a->selector);
  const char *bname = swarm_sel_getName (b->selector);
  
  return strcmp (aname, bname);
}

static int
compare_COM_objects (const void *A, const void *B, void *PARAM)
{
  ObjectEntry *a = (ObjectEntry *) A;
  ObjectEntry *b = (ObjectEntry *) B;

  if (a->foreignObject.COM < b->foreignObject.COM)
    return -1;
  
  return a->foreignObject.COM > b->foreignObject.COM;
}

static int
compare_objc_classes (const void *A, const void *B, void *PARAM)
{
  ObjectEntry *a = (ObjectEntry *) A;
  ObjectEntry *b = (ObjectEntry *) B;

  if (a->object < b->object)
    return -1;
  
  return a->object > b->object;
}

@implementation Directory
+ createBegin: aZone
{
  Directory *obj = [super createBegin: aZone];
  size_t size = sizeof (id) * DIRECTORY_SIZE;

  obj->javaTable = [aZone alloc: size];
  memset (obj->javaTable, 0, size);
  obj->selector_tree = avl_create (compare_objc_selectors, NULL);
  obj->COM_tree = avl_create (compare_COM_objects, NULL);
  obj->class_tree = avl_create (compare_objc_classes, NULL);

  return obj;
}

ObjectEntry *
swarm_directory_objc_find_object (Object_s *object)
{
  return object->foreignEntry;
}

ObjectEntry *
swarm_directory_objc_find_class (Class class)
{
  if (swarmDirectory)
    {
      ObjectEntry *ret;
      
      ret = avl_find (swarmDirectory->class_tree,
                      OBJC_FIND_OBJECT_ENTRY ((id) class));
      return ret;
    }
  return nil;
}

SelectorEntry *
swarm_directory_objc_find_selector (SEL sel)
{
  if (swarmDirectory)
    {
      SelectorEntry *ret;
      
      ret = avl_find (swarmDirectory->selector_tree,
                      OBJC_FIND_SELECTOR_ENTRY (sel));
      return ret;
    }
  return nil;
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
          ObjectEntry *ret;
          
          index = swarm_directory_java_hash_code (entry->foreignObject.java);
          m = swarmDirectory->javaTable[index];
          if (m) // no worries if it's not there; that's a BaseImpl case
            {
              
              ret = [m remove: entry];
              // don't check ret above because it may be absent from a
              // BaseImpl link
            }
          entry->object->foreignEntry = NULL;
          swarm_directory_entry_drop (entry);
          return YES;
        }
#endif
    }
  
  return NO;
}

- (void)describe: outputCharStream
{
  void node_func (void *data, void *param)
    {
      xprint (data);
    }
  avl_walk (COM_tree, node_func, NULL);

#ifdef HAVE_JDK
 {
   unsigned i;

   for (i = 0; i < DIRECTORY_SIZE; i++)
     {
       if (javaTable[i])
         {
           [outputCharStream catC: "["];
           [outputCharStream catUnsigned: i];
           [outputCharStream catC: "]:\n"];
           xfprint (javaTable[i]);
         }
     }
 }
#endif
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
      for (beg = 0; beg < end && j < 2; beg++)
        if (classname[beg] == '.') j++;
      if (j == 2) 
        len = end - beg;
      else
        {
          beg = 0;
          len = end;
        }
      strncpy (typename, &(classname[beg]), len);
      typename[len] = 0;
      {
#if SWARM_OBJC_DONE
	id ret = objc_lookup_class (typename);
#else
        id ret = swarm_objc_lookupClass (typename);
#endif
        
        // If non-CREATABLE then implementation will be set to Creating
        // and class will not be a real one, retry by appending "_c"
        if (ret && [ret isInstance] && [ret getImplementation] == Creating)
          {
            char buf[len + 2];

            stpcpy (stpcpy (buf, typename), "_c");
#if SWARM_OBJC_DONE
	    ret = objc_lookup_class (buf);
#else
            ret = swarm_objc_lookupClass (buf);
#endif
          }
        return (Class) ret;
      }
    }
#if SWARM_OBJC_DONE
  return objc_lookup_class (classname);
#else
  return swarm_objc_lookupClass (classname);
#endif
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
  COMclass cClass;

  if (swarmDirectory)
    {
      if ((cClass = COM_find_class (className)))
        objcClass = SD_COM_ENSURE_CLASS_OBJC (cClass);
#ifdef HAVE_JDK
      else
        objcClass = swarm_directory_java_find_class_named_objc (className);
#endif
    }
  if (!objcClass)
#if SWARM_OBJC_DONE
    objcClass = objc_lookup_class (className);
#else
    objcClass = swarm_objc_lookupClass (className);
#endif
  return objcClass;
}

Class
swarm_directory_swarm_class (id object)
{
  ObjectEntry *entry = swarm_directory_objc_find_object (object);
  
  if (entry)
    {
      if (entry->type == foreign_COM)
        {
          COMobject cObj = SD_COM_FIND_OBJECT_COM (object);
          COMclass cClass = COM_get_class (cObj);
          
          if (cClass)
            return SD_COM_ENSURE_CLASS_OBJC (cClass);
          else
            return Nil; // JavaScript or non-Swarm objects
        }
#ifdef HAVE_JDK
      if (entry->type == foreign_java)
        {
          jobject jobj;
          
          if ((jobj = SD_JAVA_FIND_OBJECT_JAVA (object)))
            return swarm_directory_java_class_for_object_objc (jobj);
        }
      else
#endif
        abort ();
    }
  return object ? getClass (object) : Nil;
}

Class
swarm_directory_superclass (Class class)
{
  if (SD_COM_FIND_CLASS_COM (class))
    abort ();
#ifdef HAVE_JDK
  else
    {
      jclass clazz = 0;
      
      clazz = SD_JAVA_FIND_CLASS_JAVA (class);
      
      if (clazz)
        {
          jclass javaSuperclass;
          
          javaSuperclass = (*jniEnv)->GetSuperclass (jniEnv, clazz);
          if (javaSuperclass)
            {
              Class superclass = SD_JAVA_ENSURE_CLASS_OBJC (javaSuperclass);
              
              (*jniEnv)->DeleteLocalRef (jniEnv, javaSuperclass);
              return superclass;
            }
          else
            return Nil;
        }
    }
#endif
  return swarm_class_getSuperclass (class);
}


const char *
swarm_directory_language_independent_class_name_for_objc_object  (id oObj)
{
  ObjectEntry *entry = swarm_directory_objc_find_object (oObj);
  
  if (entry)
    {
      if (entry->type == foreign_COM)
        {
          COMobject cObj;
          
          if ((cObj = SD_COM_FIND_OBJECT_COM (oObj)))
            // already copied
            return COM_class_name (cObj);
        }
#ifdef HAVE_JDK
      else if (entry->type == foreign_java)
        {
          jobject jObj;
          
          if ((jObj = SD_JAVA_FIND_OBJECT_JAVA (oObj)))
            // already copied
            return java_class_name (jObj);
        }
#endif
    }
  return (oObj
#if SWARM_OBJC_DONE
          ? (const char *) SSTRDUP ((getClass (oObj))->name)
#else
          ? (const char *) SSTRDUP (swarm_class_getName(swarm_object_getClass(oObj)))
#endif
          : NULL);
}

static const char *
language_independent_class_name_for_typename (const char *typeName, BOOL usingFlag)
{
  if (strcmp (typeName, "Create_byboth") == 0)
    return DUPCLASSNAME ("swarm/CustomizedType");
  else
    {
      extern const char *swarm_lookup_module (const char *name);
      const char *module = swarm_lookup_module (typeName);
      size_t modulelen = module ? strlen (module) + 1 : 0;
      char javaClassName[5 + 1 + modulelen + strlen (typeName) + 5 + 1];
      char *p;
      
      if (module)
        {
          p = stpcpy (javaClassName, "swarm/");
          if (modulelen > 0)
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
      else
        return NULL;
    }
}

const char *
language_independent_class_name_for_objc_class (Class oClass)
{
  const char *className;

  if ([(id) oClass isInstance])
    {
      // It should be there.  SD_COM_FIND_CLASS_COM would recurse.
      COMclass cClass = SD_COM_FIND_OBJECT_COM (oClass); 

      if (cClass)
        className = SSTRDUP (COM_get_class_name (cClass));
      else
#ifdef HAVE_JDK
        {
          // Likewise.
          jclass jClass = SD_JAVA_FIND_OBJECT_JAVA (oClass);
          
          if (jClass)
            // already copied
            className = java_get_class_name (jClass);
          else
            abort ();
        }
#else
       abort () ;
#endif
    }
  else
    {
#if SWARM_OBJC_DONE
      if (getBit (oClass->info, _CLS_DEFINEDCLASS))
#else
      if (swarm_class_getDefinedClassBit(oClass))
#endif
        {
          Type_c *typeImpl;
          Class_s *nextPhase;
          
          nextPhase = ((BehaviorPhase_s *) oClass)->nextPhase;
          typeImpl = [oClass getTypeImplemented];
          if (typeImpl)
            className =
              language_independent_class_name_for_typename (typeImpl->name,
                                                            nextPhase == NULL);
          else
            className = NULL; // e.g., when HDF5 created one
        }
      else
        {
          Type_c *typeImpl;
          
          typeImpl = [oClass getTypeImplemented];
          
          if (typeImpl)
            className =
              language_independent_class_name_for_typename (typeImpl->name, YES);
          else 
            className =
#if SWARM_OBJC_DONE
              language_independent_class_name_for_typename (oClass->name, YES);
#else
              language_independent_class_name_for_typename (swarm_class_getName(oClass), YES);
#endif
        }
    }
  return className;
}


