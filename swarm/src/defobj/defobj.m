// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         defobj.m
Description:  global data and functions for Swarm kernel
Library:      defobj
*/

#include "defobj.xm"
#import <defobj/Zone.h>
#import <collections.h>

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#include <stdio.h>

id  _obj_globalZone;
id  _obj_scratchZone;
id  _obj_initZone;
id  _obj_probeZone;

id  t_Object, t_ByteArray;

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
