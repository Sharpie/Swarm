// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Program.h
Description:  metaobjects to describe loaded elements of a program
Library:      defobj
*/

#import <defobj/DefObject.h>
#import <defobj/Symbol.h>

extern id   *_obj_classes;
extern int  _obj_nclasses;

extern void  *_obj_initAlloc( size_t size );
extern void  _obj_setTypeImplemented( id, id );

@interface InterfaceIdentifier_c : Symbol_c
@end

@interface Interface_any : Object_s
{
@public
  id  owner;          // type which owns interface
  id  messages;       // collection of message action types
  id  attributes;     // collection of attribute definitions
  id  allMessages;    // all messages including inherited ones
  id  allAttributes;  // all messages including inherited ones
}
@end

@interface Interface_c : Interface_any
{
  id  identifier;     // identifier for interface
}
@end

@interface Type_c : Interface_any
{
@public
  char  *name;            // name of type
  id    *typeID;          // global id variable containing type
  id    implementation;   // class which implements create phase of type, if any
  id    supertypes;       // other types from which type inherits
  id    redefinitions;    // chain of excluded messages (for now)
  id    interfaces;       // interfaces defined as part of type
}
- getImplementation;
- (char *) getName;
@end

@interface ProgramModule_c : Object_s
{
@public
  char  *name;     // name of module
  id    owner;     // module in which this module defined 
  id    modules;   // modules defined within this module
  id    types;     // types defined within module
  id    symbols;   // symbols (global id constants) defined within module
  id    classes;   // classes defined within module
}
/*** methods in ProgramModule_c: ***/
- (char *) getName;
- getOwner;
- getModules;
- getTypes;
- getSymbols;
- getClasses;
@end

@interface Module_super_
// Module_super_ methods:
@end
