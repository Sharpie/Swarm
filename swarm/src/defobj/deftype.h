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
Name:         deftype.h
Description:  standard definitions for protocol-based type definition
Library:      defobj
*/

//
// flag for defobj classes to inherit from objc/Object
//
#define INHERIT_OBJECT

//
// standard definitions provided by Objective C
//
#ifdef INHERIT_OBJECT
#import <objc/Object.h>
#else
#import <Swarm/swarm-objc-api.h>
#import <stdio.h>
#endif

// protocol used to declare that a defined type supports creation
@protocol CREATABLE
@end

// protocol used to declare that a defined type may be indirectly created
@protocol RETURNABLE
@end
//
// markers to define interfaces for the life cycle phases of a type
//
#define CREATING - _I_Creating;
#define SETTING  - _I_Setting;
#define USING    - _I_Using;
#define GETTERS
//
// notify_t --
//   typedef for function to be called on deallocation or reallocation of
//   an object for which a dependent reference has been registered
//
typedef void  (*notify_t)( id object, id reallocAddress, void *arg );

//
// ref_t -- typedef for a registered reference to an object
//
// (struct tag _obj_ref left undefined to leave ref_t as opaque type)
//
typedef void  *ref_t;

//
// func_t -- function pointer type
//
typedef void (*func_t)( void );

//
// fixup_t -- type of handler function to provide fixup action on an object
//
typedef id    (*fixupRoutine_t)( void *fixupContext, id fixupObject );
typedef void  (*fixup_t)( fixupRoutine_t fixupRoutine, void *fixupContext,
                          id fixupArgument );
