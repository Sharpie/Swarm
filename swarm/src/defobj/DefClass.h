// Swarm library. Copyright © 1996-2008 Swarm Development Group.
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
Name:         DefClass.h
Description:  class with variables and/or methods defined at runtime 
Library:      defobj
*/

#import <Swarm/swarm-objc-api.h>
#import <objc/Object.h>

//
// type declarations
//
typedef struct classData *classData_t;
typedef struct methodDefs *methodDefs_t;

//
// Class_s -- portion of class object allocated for all created classes 
//
@interface Class_s: Object
{
@public
  Class_s *superclass;    // object for [super ...] dispatch
  const char *name;       // character string name for class
  //long version;           // for archiving (unused)
  //unsigned long info;     // class number + info bits
  //long instanceSize;      // size of instance in bytes
  //void *ivarList;         // compiler-generated list of local ivars
  //void *methodList;       // compiler-generated list of local methods
  //struct sarray  *dtable; // dispatch table
  int somefiller;
}

/*** methods in Class_s (inserted from .m file by m2h) ***/
@end

//
// _obj_getClassData() -- function to get class data extension structure
//
extern classData_t _obj_getClassData (Class class_);

//
// _obj_initMethodInterfaces() -- generate chain of methods by interface
//
methodDefs_t _obj_initMethodInterfaces (Class class_);


//
// CreatedClass_s -- class with variables and/or methods defined at runtime 
// Does not conform to Serialization because class changes identity
// at createEnd time.
//
@interface CreatedClass_s: Class_s // Serialization
{
@public
  Class definingClass;    // compiled class defining ivar structure
  id metaobjects;         // metaobject collections
}
/*** methods in CreatedClass_s (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- setName: (const char *)className;
- setClass: (Class)aClass;
- setSuperclass: aClass;
- setDefiningClass: (Class)aClass;
- at: (SEL)aSel addMethod: (IMP)aMethod;
- lispInCreate: expr;
- hdf5InCreate: expr;
- createEnd;
- (void)hdf5OutShallow: stream;
- (void)lispOutShallow: stream;
@end

@interface BehaviorPhase_s: CreatedClass_s
{
@public
  BehaviorPhase_s *nextPhase; // class which implements next interface
  id filler;          //  pad to size of standard class (for customize)
  id morefiller;
}
/*** methods in BehaviorPhase_s (inserted from .m file by m2h) ***/
- (void)setNextPhase: aBehaviorPhase;
@end


//
// classData -- extension data for compiled class (accessed by class number)
//
struct classData {
  Class classID;                  // external id referring to class
  id owner;                       // module to which class belongs
  id typeImplemented;             // type implemented by class
  BehaviorPhase_s *initialPhase;  // class created for initial phase of type
  id metaobjects;                 // metaobject collections
  unsigned info;                  // state flags
};

//
// methodDefs -- methods of a class belonging to a named interface
//
struct methodDefs {
  methodDefs_t  next;
  id interfaceID;
#if SWARM_OBJC_DONE
  Method_t firstEntry;
#else
  ObjcMethod *firstEntry;
#endif
  int count;
};


// extended class info bits (bit masks for class->info) used by cust. wrapper

#define _CLS_DEFINEDCLASS  0x100     // info bit to mark as class created at runtime
#define _CLS_CUSTOMIZEWRAPPER 0x200  // class created by customizeBegin
#define _CLS_RETAINSELF 0x300        // retain self even if unref by createBy


//
// Functional access to the class extension data
//
static inline BOOL
swarm_class_getDefinedClassBit (Class cls)
{
  classData_t cData = _obj_getClassData(cls);
  return (cData->info & _CLS_DEFINEDCLASS) != 0;
}

static inline void
swarm_class_setDefinedClassBit (Class cls, BOOL aBit)
{
  classData_t cData = _obj_getClassData(cls);
  if (aBit) cData->info |= _CLS_DEFINEDCLASS;
  else cData->info &= ~_CLS_DEFINEDCLASS;
}
