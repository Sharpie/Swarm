// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         DefClass.m
Description:  class with variables and/or methods defined at runtime 
Library:      defobj
*/

#import <defobj/DefClass.h>
#import <defobj/Program.h>
#import <objc/objc-api.h>
#import <objc/sarray.h>

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


//
// Class_s -- portion of class object allocated for all classes 
//
@implementation Class_s
@end

//
// _obj_getClassData() -- function to get class data extension structure
//
classData_t _obj_getClassData( Class_s  *class )
{
  classData_t  classData;

  classData = (classData_t)_obj_classes[CLS_GETNUMBER( class ) - 1];
  if ( ! classData ) {
    classData = _obj_initAlloc( sizeof *classData );
    _obj_classes[CLS_GETNUMBER( class ) - 1] = (id)classData;
  }
  return classData;
}

//
// _obj_initMethodInterfaces() -- function to initialize methods by interface
//
void _obj_initMethodInterfaces( Class_s *class )
{
  classData_t   classData;
  MethodList_t  methods;
  int           count;
  id            interfaceID;
  Method_t      mnext;
  char          *mname;
  methodDefs_t  mdefs;

  classData = _obj_getClassData( class );

  for ( methods = class->methodList; methods; methods = methods->method_next ) {
    count = 0;
    interfaceID = Using;
    for ( mnext = methods->method_list + methods->method_count - 1; ;
          mnext-- ) {
      if ( mnext < methods->method_list ||
           strncmp( (mname = (char *)sel_get_name( mnext->method_name )),
                    "_I_", 3 ) == 0 ) {
        if ( count ) {
	  mdefs = _obj_initAlloc( sizeof *mdefs );
	  mdefs->next = (methodDefs_t)classData->metaobjects;
	  classData->metaobjects = (id)mdefs;
          mdefs->interfaceID = interfaceID;
	  mdefs->firstEntry  = mnext + 1;
	  mdefs->count       = count;
          if ( mnext < methods->method_list ) break;
        }
        interfaceID = mnext->method_imp( nil, (SEL)0 );
        count = 0;
      } else {
        count++;
      }
    }
  }
}


//
// CreatedClass_s -- class with variables and/or methods defined at runtime 
//

@implementation CreatedClass_s

PHASE(CreatingOnly)

+ createBegin: aZone
{
  CreatedClass_s  *newClass;

  newClass = [aZone allocIVars: self];
  return newClass;
}

- (void) setName: (char *)className
{
  name = className;
}

- (void) setClass: aClass
{
  metaobjects = aClass;
  // later -- reallocate self if class defines additional class vars
}

- (void) setSuperclass: aClass
{
  superclass = aClass;
}

- (void) setDefiningClass: aClass
{
  definingClass = aClass;
  info          = ((Class_s *)aClass)->info;
  instanceSize  = ((Class_s *)aClass)->instanceSize;
  ivarList      = ((Class_s *)aClass)->ivarList;
  methodList    = ((Class_s *)aClass)->methodList;
}

- (void) at: (SEL)aSel addMethod: (IMP)aMethod
{
  if ( ! dtable ) {
    if ( ! superclass )
      raiseEvent( InvalidCombination,
	"must specify superclass before adding methods to a created class\n" );

    dtable = sarray_lazy_copy( superclass->dtable );
  }

  sarray_at_put_safe( dtable, (size_t)aSel->sel_id, aMethod );
}

- createEnd
{
  if ( !dtable )
    dtable = sarray_lazy_copy( superclass->dtable );

  setClass( self, metaobjects );
  metaobjects = nil;

  setBit( info, _CLS_DEFINEDCLASS, 1 );
  return self;
}

@end


@implementation BehaviorPhase_s

PHASE(CreatingOnly)

- (void) setNextPhase: aBehaviorPhase
{
  nextPhase = aBehaviorPhase;
}

@end
