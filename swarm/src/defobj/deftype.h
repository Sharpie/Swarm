// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         deftype.h
Description:  standard definitions for protocol-based type definition
Library:      defobj
*/

// set flag for defobj classes to inherit from objc/Object
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

//
// macro to label an Objective C protocol as a definition of an object type 
//
#define deftype protocol

//
// protocol used to declare that a defined type supports creation
//
@protocol CREATABLE
@end

//
// markers to define interfaces for the life cycle phases of a type
//
#define CREATING - _I_Creating;
#define SETTING  - _I_Setting;
#define USING    - _I_Using;
