// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>
#include <misc.h> 
#include <misc/avl.h>

Class swarm_directory_swarm_class (id object);
const char *swarm_directory_language_independent_class_name (id object);
Class swarm_directory_ensure_class_named (const char *className);

typedef enum {
  foreign_java,
  foreign_COM
} foreign_t;

@interface DirectoryEntry: CreateDrop
{
@public
  foreign_t type;
  id object;
  union {
    JOBJECT java;
    void *COM;
  } foreignObject;
}
- setObject: object;
- setJavaObject: (JOBJECT)javaObject;
- setCOMObject: (void *)COMObject;

void swarm_directory_entry_drop (DirectoryEntry *entry);
- (void)describe: outputCharStream;
@end

@interface Directory: CreateDrop
{
@public
  id *table;
  avl_tree *objc_tree;
}
+ createBegin: aZone;
DirectoryEntry *swarm_directory_objc_find (id object);
BOOL swarm_directory_objc_remove (id obj);
- (void)describe: outputCharStream;
@end

extern Directory *swarmDirectory;

extern void swarm_directory_dump ();

#define SD_GETCLASS(obj) swarm_directory_swarm_class (obj)

#if 1
#define DIRECTORY_SIZE 21599
#else
#define DIRECTORY_SIZE 263009
#endif

extern void *alloca (size_t);

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

