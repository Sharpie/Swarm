// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Customize.h
Description:  superclass impleemntation of create-phase customization
Library:      defobj
*/

#import <defobj/DefObject.h>

//
// interface marker for methods in class which implement an interface of a type
//
#define PHASE(phase_name) \
-(id)_I_##phase_name { return phase_name; }

//
// specific defined interfaces
//
extern id Creating, Setting, Using, CreatingOnly, UsingOnly;

//
// Customize_s -- superclass impleemntation of create-phase customization
//
@interface Customize_s : Object_s // <Create>
/*** methods implemented in .m file ***/
+ customizeBegin: aZone;
- customizeEnd;
- customizeCopy: aZone;
+ customizeBeginEnd: aZone;
- _setCreateBy_: (Class)subclass message:(SEL)aSel to:anObject;
- (void) _setCreateByCopy_;
- (void) _setCreateByMessage_: (SEL)messageSelector to: anObject;
- (void) _setCreateByMessage_: (SEL)messageSelector toCopy: anObject;
- (void) _setRecustomize_: anObject;
+ (void) setTypeImplemented: aType;
@end

//
// createByCopy, createByMessageTo, createByMessageToCopy, retainSelf --
//   macros to set future create action for current customization
//

// extended class info bits (bit masks for class->info) used by cust. wrapper

#define _CLS_CUSTOMIZEWRAPPER  0x200  // class created by customizeBegin
#define _CLS_RETAINSELF        0x300  // retain self even if unref by createBy

//
// _obj_customize() -- return true if customization in progress
//
extern inline BOOL _obj_customize( id anObject )
{
  return ( getClass( anObject )->info & _CLS_CUSTOMIZEWRAPPER ) != 0;
}

#define createByCopy() \
(_obj_customize(self) ? ([(id)self _setCreateByCopy_], 1) : 0)

#define createByMessageTo( anObject, messageName ) \
(_obj_customize(self) ? \
 ([(id)self _setCreateByMessage_:@selector(messageName) to:(anObject)],1):0)

#define createByMessageToCopy( anObject, messageName ) \
(_obj_customize(self) ? \
([(id)self _setCreateByMessage_:@selector(messageName) toCopy:(anObject)],1):0)

#define setRetainSelf( ) \
if ( _obj_customize( self ) ) self->class_pointer->info |= _CLS_RETAINSELF

#define setRecustomize( recustomizeReceiver ) \
if ( _obj_customize( self ) ) [self _setRecustomize_: recustomizeReceiver]

//
// objects to save createBy actions generated customizeBegin/End
//
@interface CreateBy_c : Object_s
{
@public
  id   implementedType; // type of object created by CreateBy object
  id   createReceiver;  // receiver for message
  SEL  createMessage;   // selector from setCreateMessage:, or nil
  IMP  createMethod;    // cached method for createMessage selector
  id   recustomize;     // object to handle further create, if any
}
/*** methods implemented in .m file ***/
- createBegin: aZone;
- customizeBegin: aZone;
@end

@interface Create_bycopy : CreateBy_c
/*** methods implemented in .m file ***/
- create: aZone;
@end

@interface Create_bysend : CreateBy_c
/*** methods implemented in .m file ***/
- create: aZone;
@end

@interface Create_byboth : CreateBy_c
/*** methods implemented in .m file ***/
- create: aZone;
@end


#define _obj_NEXTCLASS 11

//
// getNextPhase() -- return class which implements next phase of object
//
extern inline Class getNextPhase( id aClass )
{
  return ((Class *)aClass)[_obj_NEXTCLASS];
}

//
// setNextPhase() -- change behavior of object to next defined phase
//
extern inline void setNextPhase( id anObject )
{
  *(Class *)anObject = (*(Class **)anObject)[_obj_NEXTCLASS];
}
