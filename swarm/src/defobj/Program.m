// Swarm library. Copyright (C) 1996-2009 Swarm Development Group.
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
Name:         Program.m
Description:  metaobjects to describe loaded elements of a program
Library:      defobj
*/

#import <defobj/Program.h>
#import <defobj/Zone.h>
#import <defobj/Symbol.h>
#import <defobj/DefClass.h>

#import <collections.h> // _collections_

#import <defobj/swarm-objc-api.h>
#import <objc/Protocol.h>

#include <misc.h> // xmalloc, strcmp, memset

// program-wide storage zones

id _obj_initZone;
externvardef id _obj_globalZone, _obj_sessionZone, _obj_scratchZone;
externvardef id _obj_GCFixedRootZone;

extern void _obj_splitPhases (Class class);

externvardef id Creating, Setting, Using, CreatingOnly, UsingOnly;

id *_obj_modules, *_obj_classes, _obj_programModule[7];
unsigned  _obj_nclasses, _obj_nmodules;

swarm_cache_ptr _obj_buckets;

id  _obj_implModule;

typedef struct objc_protocol_list *protoList_t;
typedef struct proto *proto_t;
typedef struct message *message_t;

struct proto {
  id           class;
  char         *name;
  protoList_t  protoList;
  message_t    *instanceMessages;
  message_t    *classMessages;
};

struct message {
  SEL   selector;
  char  *types;
};

//
// _obj_initAlloc() -- allocate memory during initial module definition
//
void *
_obj_initAlloc (size_t size)
{
  void  *newAlloc;

  if (_obj_initZone)
    newAlloc = [_obj_initZone allocBlock: size];
  else
    newAlloc = xmalloc (size);

  memset (newAlloc, 0, size);
  return newAlloc;
}


//
// initModules() -- initialize all modules loaded into the program
//
static void
initModules (void)
{
  Class moduleSuper, class;
  void **modules, **module;
  id *nextmod;

  // trigger initialization of superclass links and get module superclass

  moduleSuper = [Module_super_ self];

  // loop through classes to count their number and to chain module objects

#if SWARM_OBJC_DONE
  void *enumState;
  for (modules = NULL, enumState = NULL;
        (class = objc_next_class (&enumState));
       _obj_nclasses++)
    {
      if (class->super_class == moduleSuper)
        {
          _obj_nmodules++;
          // get uninitialized module object
          module = (void **) [class initialize]; 
          module[0] = (void *) modules;
          modules = module;
        }
    }
#else
  // get a list of all the classes.
  ObjcClass *classes = NULL;
  _obj_nclasses = swarm_objc_getClassList(NULL, 0);
  if (_obj_nclasses > 0) {
    classes = _obj_initAlloc (_obj_nclasses * sizeof (id));
    swarm_objc_getClassList(classes, _obj_nclasses);
  }
  //printf("# of classes: %d\n", _obj_nclasses);

  // allocate global hash table of class variable extensions
  int initialSize = 2;
  while (initialSize < _obj_nclasses) initialSize = initialSize * 2;
  _obj_buckets = swarm_hash_new(initialSize, (swarm_hash_func_type)swarm_hash_ptr,
				swarm_compare_ptrs);

  modules = NULL;
  int i;
  for (i = 0;i < _obj_nclasses; ++i) {
    class = classes[i];
    //printf("%s\n", swarm_class_getName(class));
    if (swarm_class_getSuperclass(class) == moduleSuper) {
      _obj_nmodules++;
      // get uninitialized module object
      module = (void **) [(Object *)class initialize]; 
      module[0] = (void *) modules;
      modules = module;
    }
  }
  free(classes);
#endif
  
  // allocate global array of class variable extensions by class number
  
  _obj_classes = _obj_initAlloc (_obj_nclasses * sizeof (id));
  
  // allocate and initialize global array of defined modules
  
  nextmod = _obj_modules = _obj_initAlloc (_obj_nmodules * sizeof (id));
  do
    {
      *nextmod++ = (id)modules;
      module = modules[0];
      modules[0] = nil;
      modules = module;
    } while (modules);

  // initialize _obj_initZone for use by _obj_initModule()

#if SWARM_OBJC_DONE
  _obj_initZone = _obj_initAlloc (((Class)id_Zone_c)->instance_size);
#else
  _obj_initZone = _obj_initAlloc (swarm_class_getInstanceSize(id_Zone_c));
#endif
  *(id *) _obj_initZone = id_Zone_c;
  
  // initialize interface identifier constants

  Creating = [_obj_initZone allocIVars: id_Symbol_c];
  Setting = [_obj_initZone allocIVars: id_Symbol_c];
  Using = [_obj_initZone allocIVars: id_Symbol_c];
  CreatingOnly = [_obj_initZone allocIVars: id_Symbol_c];
  UsingOnly = [_obj_initZone allocIVars: id_Symbol_c];

  ((Symbol_c *) Creating)->name = "Creating";
  ((Symbol_c *) Setting)->name = "Setting";
  ((Symbol_c *) Using)->name = "Using";
  ((Symbol_c *) CreatingOnly)->name = "CreatingOnly";
  ((Symbol_c *) UsingOnly)->name = "Using";

  // initialize file variables

  _obj_xerror = stderr;
  _obj_xdebug = stderr;

  // bootstrap initialization for symbols

  _obj_getClassData (id_CreateDrop_s)->classID = id_CreateDrop_s;
  _obj_getClassData (id_Customize_s)->classID = id_Customize_s;
  _obj_getClassData (id_EventType_c)->classID = id_EventType_c;
  _obj_getClassData (id_Symbol_c)->classID = id_Symbol_c;
  _obj_getClassData (id_Warning_c)->classID = id_Warning_c;
  _obj_getClassData (id_Error_c)->classID = id_Error_c;

  _obj_splitPhases (id_Error_c);

  Symbol  = _obj_getClassData (id_Symbol_c)->initialPhase->definingClass;
  Warning = _obj_getClassData (id_Warning_c)->initialPhase->definingClass;
  Error   = _obj_getClassData (id_Error_c)->initialPhase->definingClass;

  // initialize 

  _obj_initModule (_defobj_);

  // initialize standard allocation zones

  _obj_globalZone  = [SwarmZone create: _obj_initZone];
  _obj_sessionZone = [SwarmZone create: _obj_initZone];
  _obj_scratchZone = [SwarmZone create: _obj_initZone];

  _obj_GCFixedRootZone = [SwarmZone createBegin: _obj_initZone];
  [_obj_GCFixedRootZone setGCFixedRootFlag: YES];
  _obj_GCFixedRootZone = [_obj_GCFixedRootZone createEnd];

  // initialize collections at the end (so that it can use Zones)
  _obj_initModule (_collections_); 
}


//
// _obj_initModule() -- initialize a generated module object
//
void
_obj_initModule (void *module)
{
  ProgramModule_c  *moduleObject;
  func_t implFunction, initFunction;
  id *protocol;
  protoList_t protoList;
  proto_t *proto;
  Type_c ***typeID, *type = nil;
  char **symbol, **symbolName, symbolType = 0;
  Class **class;
  classData_t classData;
  id callerModule;

  // initialize array of all loaded modules

  if (! _obj_modules)
    initModules();
  
  // return if already initialized

  if (getClass (module))
    return;
  
  // establish module defs as an instance of ProgramModule_c
  
  moduleObject = module;
  setClass (moduleObject, id_ProgramModule_c);
  implFunction = (func_t)moduleObject->owner;    // save impl function
  initFunction = (func_t)moduleObject->modules;  // save init function
  moduleObject->owner = (id)_obj_programModule;

  //
  // create symbol objects for all defined symbols
  //

  // advance a pointer to the start of symbol names

  symbol = symbolName = (char **) moduleObject->symbols;
  for (; *symbolName; symbolName++);
  symbolName++;

  // initialize the global id constant for each symbol

  for (; *symbol; symbol++, symbolName++)
    {
      if ((*symbolName)[0] == '0')
        {
          symbolType = (*symbolName)[1];
          symbolName++;
        }
      switch (symbolType)
        {
        case 'S':
          *(id *) *symbol =
            [Symbol create: _obj_initZone setName: *symbolName];
          break;
        case 'W':
          *(id *) *symbol =
            [Warning create: _obj_initZone setName: *symbolName];
          break;
        case 'E':
          *(id *) *symbol =
            [Error create: _obj_initZone setName: *symbolName];
          break;
        default:
          abort();
        }
    }
  
  //
  // create type objects for all defined types
  //

  // advance a pointer to start of protocols for types

  protocol = (id *) moduleObject->types;
  for ( ; *protocol; protocol++);
  protocol++;
  
  // initialize the global id constant for each type

  typeID = (Type_c ***) moduleObject->types;
  for ( ; *typeID; typeID++, protocol++ )
    {
      
      // for now, just create type object with protocol saved in
      // supertypes slot
      
      **typeID = [_obj_initZone allocIVars: id_Type_c];
      type = **typeID;
      type->owner = moduleObject;
      type->name = (*(proto_t *) protocol)->name;
      type->typeID = *typeID;
      type->supertypes = *protocol;
      
      // also mark whether type is creatable based on protocol declaration
      
#if SWARM_OBJC_TODO
      for (protoList = (*(proto_t *) protocol)->protoList;
           protoList; protoList = protoList->next)
        {
          for (proto = (proto_t *) protoList->list;
               proto < (proto_t *) (protoList->list + protoList->count);
               proto++)
            {
              if ((strcmp ((*proto)->name, "CREATABLE") == 0)
                  || strcmp ((*proto)->name, "RETURNABLE") == 0)
                type->implementation = Creating;
            }
        }
#endif
    }
  
  // loop on classes to initialize owner module and class id
  
  for (class = (Class **) moduleObject->classes; *class; class++)
    {
      classData = _obj_getClassData ((Class) **class);
      if (classData->owner)
        raiseEvent (InternalError, nil);
      
      classData->owner   = moduleObject;
      classData->classID = **class;
    }
  
  // later -- initialize collections of types, symbols, and classes
  
  // call function to declare implementations for implemented module types

  callerModule = _obj_implModule;
  _obj_implModule = moduleObject;
  (*implFunction)();
  _obj_implModule = callerModule;

  // loop on classes to set external id's of creatable types

  for (class = (Class **) moduleObject->classes; *class; class++)
    {
      classData = _obj_getClassData ((Class) **class);
      
      type = classData->typeImplemented;
      if (type && type->implementation)
        {
          if (type->implementation == Creating)
            {
              if (classData->initialPhase &&
                  classData->initialPhase->nextPhase != UsingOnly)
                {
                  type->implementation = classData->initialPhase->definingClass;
                  *type->typeID = type->implementation;
                  //!! later -- use customizeBeginEnd if no extra class messages
                }
            }
          else if (classData->initialPhase->nextPhase != UsingOnly)
            {
              raiseEvent (SourceMessage,
                          "initModule(): more than one class specified as implementation\n"
                          "for Creating phase of type %s\n"
                          "(classes include %s and %s)\n",
                          [type getName], [type->implementation getName],
                          [**class getName] );
            }
        }
    }
  
  // audit that implementation provided for each creatable type interface
#if 0
  for (typeID = (Type_c ***) moduleObject->types; *typeID; typeID++)
    if ((**typeID)->implementation == Creating)
      {
        raiseEvent (WarningMessage,
                    "no implementation declared for creatable type %s\n",
                    [(id)**typeID getName]);
        type->implementation = nil;
      }
#endif

#if 0
  for (typeID = (Type_c ***) moduleObject->types; *typeID; typeID++)
    if ((**typeID)->isa == id_Type_c)
      {
        raiseEvent (WarningMessage,
                    "Creatable type %s still Type_c class\n",
                    (**typeID)->name);
      }
#endif

  // call initialize function to complete initialization of module
  
  (*initFunction) ();
}

id
defobj_lookup_type (const char *typename)
{
  unsigned i;
  
  for (i = 0; i < _obj_nmodules; i++)
    {
      id *types = (id *) [_obj_modules[i] getTypes];
      unsigned len, pos;
      
      for (len = 0; types[len]; len++);

      for (pos = 0; pos < len; pos++)
        {
          Protocol *protocol = types[pos + len + 1];
          // -name doesn't work and protocol_name is protected
          const char *name = ((const char **) protocol)[1];
          
          if (strcmp (name, typename) == 0)
            return *(id *) types[pos];
        }
      types++;
      
    }
  return nil;
}


//
// Type_c -- type object generated for @protocol definition in header
//

@implementation Type_c
PHASE(Using)
//
// getImplementation: -- return create-phase implementation of a creatable type
//
- (BOOL)getCreatable
{
  return implementation != nil;
}

- getImplementation
{
  return implementation;
}

- (const char *)getName
{
  return SSTRDUP (name);
}

@end


//
// ProgramModule_c -- module object generated for a library module
//

@implementation ProgramModule_c

- (const char *)getName  // accessor method
{
  return SSTRDUP (name);
}

- getOwner  // accessor method
{
  return owner;
}

- getModules  // accessor method
{
  if (!modules);  // initialize modules collection
  return modules;
}

- getTypes  // accessor method
{
  // initialize types array 
  return types;
}

- getSymbols  // accessor method
{
  // initialize symbols array
  return symbols;
}

- getClasses  // accessor method
{
  // initialize classes array
  return classes;
}

@end


//
// Module_super_ -- superclass used to access generated module definitions
//

@implementation Module_super_

+ initialize
{
	return self;
}

+ self
{
  return self;
}

@end
