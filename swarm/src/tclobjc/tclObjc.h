/* Interface for Objective-C Tcl interpreter functions
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

#ifndef _tclObjc_h
#define _tclObjc_h

#include <objc/objc.h>
#include <tcl.h>

#define TCLOBJC_VERSION "1.3"
extern const char tclObjcVersion[];

extern int TclObjc_Init(Tcl_Interp *interp);

#define TCLOBJC_NO_OBJ ((id)-1)

extern char *tclObjc_objectToName(id obj);
extern id tclObjc_nameToObject(const char *name);
extern void 
tclObjc_registerObjectWithName(Tcl_Interp *interp, 
			       id object, const char *name);
extern void 
tclObjc_unregisterObjectNamed(Tcl_Interp *interp,
			      const char *name);
extern int 
tclObjc_msgSendToClientData(ClientData clientData, 
			   Tcl_Interp *interp, int argc, char *argv[]);
int 
tclObjc_msgSendToArgv1(ClientData clientData, Tcl_Interp *interp,
		       int argc, char *argv[]);

extern int (*tclObjc_eventHook)();

#ifdef NeXT
@interface TclObject:Object
#else
@interface TclObject
#endif
{
#ifdef GCCLT270
  Class *ISA;
#else
  Class ISA;
#endif
  @public
  Tcl_Interp *_interp;
  char *_tclName;
}
+ newName: (char *)objectName;
- performv:(SEL)aSel :(arglist_t)argframe;
- forward: (SEL)aSel : (arglist_t)argframe;
@end

#endif /* _tclObjc_h */

