// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

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
#import <objc/objc.h>
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
