// Swarm library. Copyright (C) 1996-1997-1997 Santa Fe Institute.
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
  // initialize error messages

  [InvalidCombination setMessageString:
"> Customization messages sent to a type are incompatible with each other\n"
"> or with other requirements of the type.\n" ];

  [InvalidArgument setMessageString:
"> Invalid argument value passed in call.\n"];

  [OutOfMemory setMessageString:
"> No more memory available from the system.  Value of sbrk: %0#8x\n"];

  [InvalidAllocSize setMessageString:
"> Requested allocation size must be at least one byte.\n"
"> (Requested allocation size was zero.)\n"];

  [BlockedObjectAlloc setMessageString:
"> Requested operation is defined by the Object superclass of the GNU\n"
"> Objective C runtime system, but is blocked from usage because its default\n"
"> implementation is incompatible with the model of zone-based allocation\n"
"> established by the defobj package.  To allocate, free, or copy objects\n"
"> that use the defined model of zone-based allocation, one of the messages\n"
"> create:, createBegin:/End, drop, or copy: must be used instead.\n"];

  [BlockedObjectUsage setMessageString:
"> Requested operation is implemented by the Object superclass of the GNU\n"
"> Objective C runtime system, but is blocked from usage because it is not\n"
"> part of the standard public view established by the defobj package.\n"
"> See documentation for an explanation of the supported public view,\n"
"> including functional equivalents for most messages defined by the Object\n"
"> superclass.  This error may be avoided by compiling the defobj library\n"
"> without the -DINHERIT_OBJECT_WITH_ERRORS compile-time flag set.\n"];
}
