// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         DefClass.h
Description:  class with variables and/or methods defined at runtime 
Library:      defobj
*/

#import <defobj/Create.h>
#import <objc/Object.h>
#import <objc/objc-api.h>

//
// type declarations
//
typedef struct classData   *classData_t;
typedef struct methodDefs  *methodDefs_t;

//
// Class_s -- portion of class object allocated for all created classes 
//
@interface Class_s : Object
{
@public
  Class_s        *superclass;    // object for [super ...] dispatch
  const char     *name;          // character string name for class
  long           version;        // for archiving (unused)
  unsigned long  info;           // class number + info bits
  long           instanceSize;   // size of instance in bytes
  void           *ivarList;      // compiler-generated list of local ivars
  void           *methodList;    // compiler-generated list of local methods
  struct sarray  *dtable;        // dispatch table
}
/*** methods in Class_s (inserted from .m file by m2h) ***/
@end

//
// _obj_getClassData() -- function to get class data extension structure
//
extern classData_t _obj_getClassData( Class_s *class );

//
// _obj_initMethodInterfaces() -- generate chain of methods by interface
//
void _obj_initMethodInterfaces( Class_s *class );


//
// CreatedClass_s -- class with variables and/or methods defined at runtime 
//
@interface CreatedClass_s : Class_s
{
@public
  Class_s  *definingClass;  // compiled class defining ivar structure
  id       metaobjects;     // metaobject collections
}
/*** methods in CreatedClass_s (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- (void) setName: (const char *)className;
- (void) setClass: aClass;
- (void) setSuperclass: aClass;
- (void) setDefiningClass: aClass;
- (void) at: (SEL)aSel addMethod: (IMP)aMethod;
- createEnd;
@end

@interface BehaviorPhase_s : CreatedClass_s
{
@public
  Class_s  *nextPhase;   // class which implements next interface
  id       filler;       //  pad to size of standard class (for customize)
}
/*** methods in BehaviorPhase_s (inserted from .m file by m2h) ***/
- (void) setNextPhase: aBehaviorPhase;
@end

// info bit to mark as class created at runtime
#define _CLS_DEFINEDCLASS  0x100

//
// classData -- extension data for compiled class (accessed by class number)
//
struct classData {
  id               *classID;         // external id referring to class
  id               owner;            // module to which class belongs
  id               typeImplemented;  // type implemented by class
  BehaviorPhase_s  *initialPhase;    // class created for initial phase of type
  id               metaobjects;      // metaobject collections
};

//
// methodDefs -- methods of a class belonging to a named interface
//
struct methodDefs {
  methodDefs_t  next;
  id            interfaceID;
  Method_t      firstEntry;
  int           count;
};
