// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>
#include <misc.h> 
#include <misc/avl.h>

Class swarm_directory_swarm_class (id object);
const char *swarm_directory_language_independent_class_name_for_objc_object (id object);
const char *language_independent_class_name_for_objc_class (Class class);

Class swarm_directory_ensure_class_named (const char *className);

typedef enum {
  foreign_java,
  foreign_COM
} foreign_t;

@interface DirectoryEntry: CreateDrop
{
@public
  foreign_t type;
  union {
    JOBJECT java;
    COMOBJECT *COM;
  } foreignObject;
}
- setCOMObject: (COMOBJECT)COMObject;
- setJavaObject: (JOBJECT)javaObject;
void swarm_directory_entry_drop (DirectoryEntry *entry);
- (void)describe: outputCharStream;
@end

@interface ObjectEntry: DirectoryEntry
{
@public
  id object;
}
- setObject: object;
@end

@interface SelectorEntry: DirectoryEntry
{
@public
  SEL selector;
}
- setSelector: (SEL)sel;
@end

@interface Directory: CreateDrop
{
@public
  id *javaTable;
  avl_tree *COM_tree;
  avl_tree *object_tree;
  avl_tree *selector_tree;
}
+ createBegin: aZone;
ObjectEntry *swarm_directory_objc_find_object (id object);
SelectorEntry *swarm_directory_objc_find_selector (SEL sel);
BOOL swarm_directory_objc_remove (id obj);
- (void)describe: outputCharStream;
@end

extern Directory *swarmDirectory;

extern void swarm_directory_dump ();

#define SD_GETCLASS(obj) swarm_directory_swarm_class (obj)

extern Class swarm_directory_superclass (Class class);
#define SD_SUPERCLASS(class) swarm_directory_superclass (class)


#if 1
#define DIRECTORY_SIZE 21599
#else
#define DIRECTORY_SIZE 263009
#endif

extern void *alloca (size_t);

#define OBJC_FIND_OBJECT_ENTRY(theObject) ({ ObjectEntry *_findEntry  = alloca (sizeof (ObjectEntry)); _findEntry->object = theObject; _findEntry->foreignObject.java = 0; _findEntry; })

#define OBJC_FIND_SELECTOR_ENTRY(theSel) ({ SelectorEntry *_findEntry  = alloca (sizeof (SelectorEntry)); _findEntry->selector = theSel; _findEntry->foreignObject.java = 0; _findEntry; })

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

