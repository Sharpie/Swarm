// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Program.m
Description:  metaobjects to describe loaded elements of a program
Library:      defobj
*/

#import <defobj/Program.h>
#import <defobj/Zone.h>
#import <defobj/Symbol.h>
#import <defobj/DefClass.h>
#import <collections.h>

#import <objc/objc-api.h>
#import <objc/Protocol.h>

#define __USE_FIXED_PROTOTYPES__ // for gcc headers
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void _obj_splitPhases( Class_s  *class );

id   Creating, Setting, Using, CreatingOnly, UsingOnly;

id   *_obj_modules, *_obj_classes, _obj_programModule[7];
int  _obj_nclasses, _obj_nmodules;

id  _obj_implModule;

typedef struct objc_protocol_list  *protoList_t;
typedef struct proto               *proto_t;
typedef struct message             *message_t;

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
void *_obj_initAlloc( size_t size )
{
  void  *newAlloc;

  if ( _obj_initZone )
    newAlloc = [_obj_initZone allocBlock: size];
  else
    newAlloc = malloc( size );

  memset( newAlloc, 0, size );
  return newAlloc;
}


//
// initModules() -- initialize all modules loaded into the program
//
static void initModules( void )
{
  Class  moduleSuper, class;
  void   *enumState, **modules, **module;
  id     *nextmod;

  // trigger initialization of superclass links and get module superclass

  moduleSuper = [Module_super_ self];

  // loop through classes to count their number and to chain module objects

  for ( modules = NULL, enumState = NULL;
        (class = objc_next_class( &enumState ));
        _obj_nclasses++ ) {
    if ( class->super_class == moduleSuper ) {
      _obj_nmodules++;
      module = (void **)[class initialize];  // get uninitialized module object
      module[0] = (void *)modules;
      modules = module;
    }
  }

  // allocate global array of class variable extensions by class number

  _obj_classes = _obj_initAlloc( _obj_nclasses * sizeof (id) );

  // allocate and initialize global array of defined modules

  nextmod = _obj_modules = _obj_initAlloc( _obj_nmodules * sizeof (id) );
  do {
    *nextmod++ = (id)modules;
    module     = modules[0];
    modules[0] = nil;
    modules    = module;
  } while ( modules );

  // initialize _obj_initZone for use by _obj_initModule()

  _obj_initZone = _obj_initAlloc( ((Class)id_Zone_c)->instance_size );
  *(id *)_obj_initZone = id_Zone_c;

  // initialize interface identifier constants

  Creating     = [_obj_initZone allocIVars: id_Symbol_c];
  Setting      = [_obj_initZone allocIVars: id_Symbol_c];
  Using        = [_obj_initZone allocIVars: id_Symbol_c];
  CreatingOnly = [_obj_initZone allocIVars: id_Symbol_c];
  UsingOnly    = [_obj_initZone allocIVars: id_Symbol_c];

  ((Symbol_c *)Creating    )->name = "Creating";
  ((Symbol_c *)Setting     )->name = "Setting";
  ((Symbol_c *)Using       )->name = "Using";
  ((Symbol_c *)CreatingOnly)->name = "CreatingOnly";
  ((Symbol_c *)UsingOnly   )->name = "Using";

  // initialize file variables

  _obj_xerror = stderr;
  _obj_xdebug = stderr;

  // bootstrap initialization for symbols

  _obj_getClassData( id_CreateDrop_s  )->classID = &id_CreateDrop_s;
  _obj_getClassData( id_Customize_s   )->classID = &id_Customize_s;
  _obj_getClassData( id_EventType_c   )->classID = &id_EventType_c;
  _obj_getClassData( id_Symbol_c      )->classID = &id_Symbol_c;
  _obj_getClassData( id_Warning_c     )->classID = &id_Warning_c;
  _obj_getClassData( id_Error_c       )->classID = &id_Error_c;

  _obj_splitPhases( id_Error_c );

  Symbol  = _obj_getClassData( id_Symbol_c  )->initialPhase;
  Warning = _obj_getClassData( id_Warning_c )->initialPhase;
  Error   = _obj_getClassData( id_Error_c   )->initialPhase;

  // initialize 

  _obj_initModule( _defobj_ );
}


//
// _obj_initModule() -- initialize a generated module object
//
void _obj_initModule( void *module )
{
  ProgramModule_c  *moduleObject;
  func_t           implFunction, initFunction;
  id               *protocol;
  protoList_t      protoList;
  proto_t          *proto;
  Type_c           ***typeID, *type = nil;
  char             **symbol, **symbolName, symbolType = 0;
  Class            **class;
  classData_t      classData;
  id               callerModule;

  // initialize array of all loaded modules

  if ( ! _obj_modules ) initModules();

  // return if already initialized

  if ( getClass( module ) ) return;

  // establish module defs as an instance of ProgramModule_c

  moduleObject = module;
  setClass( moduleObject, id_ProgramModule_c );
  implFunction        = (func_t)moduleObject->owner;    // save impl function
  initFunction        = (func_t)moduleObject->modules;  // save init function
  moduleObject->owner = (id)_obj_programModule;

  //
  // create symbol objects for all defined symbols
  //

  // advance a pointer to the start of symbol names

  symbol = symbolName = (char **)moduleObject->symbols;
  for ( ; *symbolName; symbolName++ );
  symbolName++;

  // initialize the global id constant for each symbol

  for ( ; *symbol; symbol++, symbolName++ ) {
    if ( (*symbolName)[0] == '0' ) {
      symbolType = (*symbolName)[1];
      symbolName++;
    }
    switch ( symbolType ) {
    case 'S':
      *(id *)*symbol = [Symbol create: _obj_initZone setName: *symbolName];
      break;
    case 'W':
      *(id *)*symbol = [Warning create: _obj_initZone setName: *symbolName];
      break;
    case 'E':
      *(id *)*symbol = [Error create: _obj_initZone setName: *symbolName];
      break;
    default:
      fprintf( stderr, "error in generated symbols for module: %s\n",
               moduleObject->name );
      abort();
    }
  }

  //
  // create type objects for all defined types
  //

  // advance a pointer to start of protocols for types

  protocol = (id *)moduleObject->types;
  for ( ; *protocol; protocol++ );
  protocol++;

  // initialize the global id constant for each type

  typeID = (Type_c ***)moduleObject->types;
  for ( ; *typeID; typeID++, protocol++ ) {

    // for now, just create type object with protocol saved in supertypes slot

    **typeID = [_obj_initZone allocIVars: id_Type_c];
    type                 = **typeID;
    type->owner          = moduleObject;
    type->name           = (*(proto_t *)protocol)->name;
    type->typeID         = *typeID;
    type->supertypes     = *protocol;

    // also mark whether type is creatable based on protocol declaration

    for ( protoList = (*(proto_t *)protocol)->protoList;
          protoList; protoList = protoList->next ) {
      for ( proto = (proto_t *)protoList->list;
            proto < (proto_t *)( protoList->list + protoList->count );
            proto++ ) {
        if ( strcmp( (*proto)->name, "CREATABLE" ) == 0 )
          type->implementation = Creating;
      }
    }
  }

  // loop on classes to initialize owner module and class id

  for ( class = (Class **)moduleObject->classes; *class; class++ ) {
    classData = _obj_getClassData( (Class_s *)**class );
    if ( classData->owner ) raiseEvent( InternalError, nil );

    classData->owner   = moduleObject;
    classData->classID = *class;
  }

  // later -- initialize collections of types, symbols, and classes

  // call function to declare implementations for implemented module types

  callerModule = _obj_implModule;
  _obj_implModule = moduleObject;
  (*implFunction)();
  _obj_implModule = callerModule;

  // loop on classes to set external id's of creatable types

  for ( class = (Class **)moduleObject->classes; *class; class++ ) {
    classData = _obj_getClassData( (Class_s *)**class );

    type = classData->typeImplemented;
    if ( type && type->implementation ) {
      if ( type->implementation == Creating ) {
        if ( classData->initialPhase &&
             classData->initialPhase->nextPhase != UsingOnly ) {
	  type->implementation = classData->initialPhase;
	  *type->typeID = type->implementation;
	  //!! later -- use customizeBeginEnd if no extra class messages
        }
      } else if ( classData->initialPhase->nextPhase != UsingOnly ) {
	raiseEvent( SourceMessage,
	  "initModule(): more than one class specified as implementation\n"
	  "for Creating phase of type %s\n"
	  "(classes include %s and %s)\n",
	  [type getName], [type->implementation getName],
	  [**class getName] );
      }
    }
  }

  // audit that implementation provided for each creatable type interface

  for ( typeID = (Type_c ***)moduleObject->types; *typeID; typeID++ )
    if ( (**typeID)->implementation == Creating ) {
      raiseEvent( WarningMessage,
	"no implementation declared for creatable type %s\n",
	[(id)**typeID getName] );
      type->implementation = nil;
    }

  // call initialize function to complete initialization of module

  (*initFunction)();
}


//
// Type_c -- type object generated for @deftype definition in header
//

@implementation Type_c

//
// getImplementation: -- return create-phase implementation of a creatable type
//
- (BOOL) getCreatable
{
  return implementation != nil;
}

- getImplementation
{
  return implementation;
}

- (char *) getName
{
  return name;
}

@end


//
// ProgramModule_c -- module object generated for a library module
//

@implementation ProgramModule_c

- (char *) getName  // accessor method
{
  return name;
}

- getOwner  // accessor method
{
  return owner;
}

- getModules  // accessor method
{
  if ( ! modules ) ;  // initialize modules collection
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

+ self
{
  return self;
}

@end
