/* Interface for Objective-C Tcl interpreter object
   Copyright (C) 1993,1994  R. Andrew McCallum

   Written by:  R. Andrew McCallum <mccallum@cs.rochester.edu>
   Dept. of Computer Science, U. of Rochester, Rochester, NY  14627

   This file is part of the Tcl/Objective-C interface library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/ 

#ifndef _TclInterp_h
#define _TclInterp_h

#include <objc/Object.h>
#include <tcl.h>
#include <tclObjc.h>

#if !((TCL_MAJOR_VERSION == 7 && TCL_MINOR_VERSION >= 4) || TCL_MAJOR_VERSION >= 8)
#   define TCLVERSIONLT74 1
#else
#   define TCLVERSIONLT74 0
#endif
#if ((TCL_MAJOR_VERSION == 7 && TCL_MINOR_VERSION > 4) || TCL_MAJOR_VERSION >= 8)
#   define TCLVERSIONGT74 1
#else
#   define TCLVERSIONGT74 0
#endif



@interface TclInterp: Object
{
  @public
  Tcl_Interp *interp;
  int code;
  id namesToObjects;
  id objectsToNames;
  BOOL evalDebugPrint;
  const char *secondaryPath;
}

+ initialize;
+ firstTcl;
+ tclAtIndex: (unsigned)index;
+ (unsigned) tclCount;

- initWithArgc: (int)argc argv: (const char **)argv;
- setSecondaryLibraryPath: (const char *)path;
- (const char *)checkTclLibrary;
- (BOOL)checkPath: (const char *)base file: (const char *)file;
- (const char *) preInitWithArgc: (int)argc argv: (const char **)argv;
- init;
- free;

- eval: (const char *)fmt, ...;
- globalEval: (const char *)fmt, ...;
- evalFile: (const char *)filename;

- promptAndEval;

- (BOOL)variableExists: (const char *)varName;
- (const char *)variableValue: (const char *)varName;
- (BOOL) globalVariableExists: (const char *)varName;
- (const char *)globalVariableValue: (const char *)varName;
- (int)code;
- (const char *)result;
- (Tcl_Interp *)interp;

- registerObject: anObject withName: (const char *)aName;
- unregisterObject: anObject;
- unregisterObjectNamed: (const char *)aName;
- (const char *)nameForObject:anObject;
- objectNamed: (const char *)aName;
- (BOOL)objectIsRegistered: anObject;
- (BOOL)nameIsRegistered: (const char *)aName;

- setEvalDebugPrint: (BOOL) value;

@end

#endif /* _TclInterp_h */
