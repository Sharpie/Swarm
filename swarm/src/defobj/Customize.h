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
Name:         Customize.h
Description:  superclass impleemntation of create-phase customization
Library:      defobj
*/

#import <Swarm/DefClass.h> // BehaviorPhase_s
#import <Swarm/DefObject.h>
#import <Swarm/swarm-objc-api.h>

//
// interface marker for methods in class which implement an interface of a type
//
#define PHASE(phase_name) \
-(id)_I_##phase_name { return phase_name; } \
+(id)_C_##phase_name { return phase_name; }

//
// specific defined interfaces
//
externvar id Creating, Setting, Using, CreatingOnly, UsingOnly;

//
// Customization wrapper
//
struct customizeWrapper {
  Class class;
  int flags;
  id createBy;
};

//
// Customize_s -- superclass impleemntation of create-phase customization
//
@interface Customize_s: Object_s
{
@public
  struct customizeWrapper *wrapper;
}
+ createBegin: aZone;
/*** methods in Customize_s (inserted from .m file by m2h) ***/
+ customizeBegin: aZone;
- customizeEnd;
- customizeCopy: aZone;
+ customizeBeginEnd: aZone;
- _setCreateBy_: (Class)subclass message: (SEL)messageSelector to: anObject;
- (void)_setCreateByCopy_;
- (void)_setCreateByMessage_: (SEL)messageSelector to: anObject;
- (void)_setCreateByMessage_: (SEL)messageSelector toCopy: anObject;
- (void)_setRecustomize_: anObject;
+ (void)setTypeImplemented: aType;
@end

//
// createByCopy, createByMessageTo, createByMessageToCopy, retainSelf --
//   macros to set future create action for current customization
//

//
// _obj_customize() -- return true if customization in progress
//
extern inline BOOL
_obj_customize (Customize_s *anObject)
{
#if SWARM_OBJC_DONE
  return (getClass (anObject)->info & _CLS_CUSTOMIZEWRAPPER) != 0;
#else
  if ((anObject->wrapper) && (anObject->wrapper->flags & _CLS_CUSTOMIZEWRAPPER))
    return YES;
  return NO;
#endif
}

#define createByCopy() \
(_obj_customize (self) ? ([(id) self _setCreateByCopy_], YES) : NO)

#define createByMessageTo(anObject, messageName) \
(_obj_customize (self) ? \
 ([(id) self _setCreateByMessage_ : @selector(messageName) to: (anObject)], YES) : NO)

#define createByMessageToCopy(anObject, messageName) \
(_obj_customize (self) ? \
([(id) self _setCreateByMessage_: @selector(messageName) toCopy: (anObject)], YES) : NO)

#define setRetainSelf() \
if (_obj_customize (self)) self->wrapper->flags |= _CLS_RETAINSELF

#define setRecustomize(recustomizeReceiver) \
if (_obj_customize(self)) [self _setRecustomize_: recustomizeReceiver]

//
// objects to save createBy actions generated customizeBegin/End
//
@interface CreateBy_c: Object_s
{
@public
  id implementedType; // type of object created by CreateBy object
  id createReceiver;  // receiver for message
  SEL createMessage;  // selector from setCreateMessage:, or nil
  IMP createMethod;   // cached method for createMessage selector
  id recustomize;     // object to handle further create, if any
}
/*** methods in CreateBy_c (inserted from .m file by m2h) ***/
- createBegin: aZone;
- customizeBegin: aZone;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end

@interface Create_bycopy: CreateBy_c
/*** methods in Create_bycopy (inserted from .m file by m2h) ***/
- create: aZone;
@end

@interface Create_bysend: CreateBy_c
/*** methods in Create_bysend (inserted from .m file by m2h) ***/
- create: aZone;
@end

@interface Create_byboth: CreateBy_c
/*** methods in Create_byboth (inserted from .m file by m2h) ***/
- create: aZone;
@end

//
// getNextPhase() -- return class which implements next phase of object
//
extern Class
getNextPhase (Class aClass);

//
// setNextPhase() -- change behavior of object to next defined phase
//
extern void
setNextPhase (id anObject);

