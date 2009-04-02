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

#import <Swarm/Create.h>
#include <Swarm/misc.h> 
#include <Swarm/avl.h>

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
    COMOBJECT COM;
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
  Object_s *object;
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
  avl_tree *class_tree;
  avl_tree *selector_tree;
}
+ createBegin: aZone;
ObjectEntry *swarm_directory_objc_find_object (Object_s *object);
ObjectEntry *swarm_directory_objc_find_class (Class class);
SelectorEntry *swarm_directory_objc_find_selector (SEL sel);
BOOL swarm_directory_objc_remove (id obj);
- (void)describe: outputCharStream;
@end

extern Directory *swarmDirectory;

extern void swarm_directory_dump ();

Class swarm_directory_swarm_class (id obj);
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

