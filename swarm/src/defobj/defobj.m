// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         defobj.m
Description:  global data and functions for Swarm kernel
Library:      defobj
*/

#include "defobj.xm"
#import  <defobj/Zone.h>

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#include <stdio.h>

id  _obj_globalZone;
id  _obj_scratchZone;
id  _obj_initZone;
id  _obj_probeZone;

BOOL  _warning_dropFrom = 1;

BOOL _obj_debug = 1;
FILE *_obj_xerror, *_obj_xdebug;

//
// _defobj_implement() -- generate implementations for defobj module
//
void _defobj_implement( void )
{
  [id_Zone_c          setTypeImplemented: Zone];
  [id_Symbol_c        setTypeImplemented: Symbol];
  [id_Warning_c       setTypeImplemented: Warning];
  [id_Error_c         setTypeImplemented: Error];
  [id_CreatedClass_s  setTypeImplemented: CreatedClass];
  [id_BehaviorPhase_s setTypeImplemented: BehaviorPhase];
}

//
// _defobj_initialize() -- initialize global data for defobj module
//
void _defobj_initialize( void )
{
  // initialize standard allocation zones

  _obj_globalZone  = [Zone create: _obj_initZone];
  _obj_scratchZone = [Zone create: _obj_initZone];
  _obj_probeZone   = [Zone create: _obj_initZone];

  // initialize error messages

  [InvalidCombination setMessageString:
    "Customization messages sent to a type are incompatible with each other\n"
    "or with other requirements of the type.\n" ];
  [InvalidArgument setMessageString:
    "Invalid argument value passed in call.\n"];

  [OutOfMemory setMessageString:
   "No more memory available from the system.  Value of sbrk: %0#8x\n"];

  [InvalidAllocSize setMessageString:
    "Requested allocation size must be at least one byte.\n"
    "(Requested allocation size was zero.)\n"];
}

//
// xprint() -- print the debug description of an object
//
void
xprint( id anObject )
{
  if ( anObject ) {
    fprintf( _obj_xdebug,
	     "object is %0#8x: %s\n",
	     (unsigned int)anObject, [[anObject getClass] getName] );
  } else {
    fprintf( _obj_xdebug, "object is nil\n" );
  }
}

//
// xfprint() -- print the debug description for each member of a collection
//
void
xfprint( id aCollection )
{
  id index, member;

  if ( aCollection ) {
    index = [aCollection begin: scratchZone];
    while ( (member = [index next]) ) xprint( member );
  } else {
    fprintf( _obj_xdebug, "collection is nil\n" );
  }
}

//
// xexec() -- perform a message on an object
//
void
xexec( id anObject, char *msgName )
{
  SEL sel;

  if ( anObject ) {
    sel = sel_get_any_uid( msgName );
    if ( sel ) {
      if ( [anObject respondsTo: sel] ) {
	[anObject perform: sel];
      } else {
        fprintf( _obj_xdebug,
                "Object %0#8x: %s does not respond to message %s\n",
              (unsigned int)anObject, [[anObject getClass] getName], msgName );
      }
    } else {
      fprintf( _obj_xdebug, "message \"%s\" is not defined\n", msgName );
    }
  } else {
    fprintf( _obj_xdebug, "object is nil" );
  }
}

//
// xexec() -- perform a message on each message of a collection
//
void
xfexec( id anObject, char *msgName )
{
  SEL sel;

  if ( anObject ) {
    sel = sel_get_any_uid( msgName );
    if ( sel ) {
      if ( [anObject respondsTo: @selector( forEach: )] ) {
	[anObject forEach: sel];
      } else {
        fprintf( _obj_xdebug,
                "Object %0#8x: %s does not respond to message forEach:\n",
                 (unsigned int)anObject, [[anObject getClass] getName] );
      }
    } else {
      fprintf( _obj_xdebug, "message \"%s\" is not defined\n", msgName );
    }
  } else {
    fprintf( _obj_xdebug, "collection is nil" );
  }
}
