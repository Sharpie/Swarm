// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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

/*
Name:         Program.h
Description:  metaobjects to describe loaded elements of a program
Library:      defobj
*/

#import <Swarm/DefObject.h>
#import <Swarm/Symbol.h>
#import <Swarm/swarm_hash.h>

extern id *_obj_classes;
extern unsigned _obj_nclasses;

extern swarm_cache_ptr _obj_buckets;

extern void *_obj_initAlloc (size_t size);
extern void _obj_setTypeImplemented (id, id);

@interface Type_c: Object_s
{
@public
  id owner;            // module that contains type
  const char *name;    // name of type
  id *typeID;          // global id variable containing type
  id implementation;   // class that implements create phase, if any
  id supertypes;       // other types from which type inherits
}
/*** methods in Type_c (inserted from .m file by m2h) ***/
- (BOOL)getCreatable;
- getImplementation;
- (const char *)getName;
@end

@interface ProgramModule_c: Object
{
@public
  const char *name; // name of module
  id owner;         // module in which this module defined 
  id modules;       // modules defined within this module
  id types;         // types defined within module
  id symbols;       // symbols (global id constants) defined within module
  id classes;       // classes defined within module
}
/*** methods in ProgramModule_c (inserted from .m file by m2h) ***/
- (const char *)getName;
- getOwner;
- getModules;
- getTypes;
- getSymbols;
- getClasses;
@end

id defobj_lookup_type (const char *typename);

#if SWARM_OSX
@interface Module_super_ : NSObject
#else
@interface Module_super_
#endif
/*** methods in Module_super_ (inserted from .m file by m2h) ***/
+ self;
@end
