// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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
#import <collections.h> // catC:

#include <misc.h> // strncmp

//
// Class_s -- portion of class object allocated for all classes 
//
@implementation Class_s
@end

//
// _obj_getClassData() -- function to get class data extension structure
//
classData_t
_obj_getClassData (Class_s *class)
{
  classData_t classData;

  classData = (classData_t)_obj_classes[CLS_GETNUMBER (class) - 1];
  if (!classData)
    {
      classData = _obj_initAlloc (sizeof *classData);
      _obj_classes[CLS_GETNUMBER (class) - 1] = (id) classData;
    }
  return classData;
}

//
// _obj_initMethodInterfaces() -- function to initialize methods by interface
//
void
_obj_initMethodInterfaces (Class_s *class)
{
  classData_t   classData;
  MethodList_t  methods;
  int           count;
  id            interfaceID;
  Method_t      mnext;
  const char    *mname;
  methodDefs_t  mdefs;

  classData = _obj_getClassData (class);
  
  for (methods = class->methodList; methods; methods = methods->method_next)
    {
      count = 0;
      interfaceID = Using;
      for (mnext = methods->method_list + methods->method_count - 1; ; mnext--)
        {
          if (mnext < methods->method_list
              || strncmp ((mname =
                           (const char *) sel_get_name (mnext->method_name)),
                          "_I_", 3 ) == 0)
            {
              if (count)
                {
                  mdefs = _obj_initAlloc( sizeof *mdefs );
                  mdefs->next = (methodDefs_t)classData->metaobjects;
                  classData->metaobjects = (id)mdefs;
                  mdefs->interfaceID = interfaceID;
                  mdefs->firstEntry = mnext + 1;
                  mdefs->count = count;
                  if (mnext < methods->method_list)
                    break;
                }
              interfaceID = mnext->method_imp (nil, (SEL)0);
              count = 0;
            }
          else
            count++;
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
  return [aZone allocIVars: self];
}

- setName: (const char *)className
{
  name = className;
  return self;
}

- (void)setClass: aClass
{
  metaobjects = aClass;
  // later -- reallocate self if class defines additional class vars
}

- (void)setSuperclass: aClass
{
  superclass = aClass;
}

- (void)setDefiningClass: aClass
{
  definingClass = aClass;
  info = ((Class_s *) aClass)->info;
  instanceSize = ((Class_s *) aClass)->instanceSize;
  ivarList = ((Class_s *) aClass)->ivarList;
  methodList = ((Class_s *) aClass)->methodList;
}

struct objc_ivar_list *
allocate_ivar_list (struct objc_ivar_list *ivars, unsigned additional)
{
  unsigned existing = ivars ? ivars->ivar_count : 0;
  unsigned count = existing + additional;
  struct objc_ivar_list *newivars =
    xmalloc (sizeof (struct objc_ivar_list) +
	     (count - 1) * sizeof (struct objc_ivar));
  if (existing > 0)
    memcpy (newivars->ivar_list, ivars->ivar_list,
	    existing * sizeof (struct objc_ivar));
  newivars->ivar_count = existing;
  return newivars;
}

static size_t
align (size_t pos, size_t alignment)
{
  size_t mask = (alignment - 1);

  if ((pos & mask) == 0)
    return pos;
  else
    return (pos + alignment) & ~mask;
}

id
addVariable (id class, const char *varName, const char *varType)
{
  struct objc_ivar_list *ivars =
    allocate_ivar_list (((Class_s *) class)->ivarList, 1);
  Class_s *newClass;
  size_t classSize = sizeof (struct objc_class);

  newClass = xmalloc (classSize);
  memcpy (newClass, class, classSize);

  newClass->ivarList = ivars;
    
  {
    struct objc_ivar *il = &ivars->ivar_list[ivars->ivar_count];
    size_t alignment, size;

    switch (*varType)
      {
      case _C_INT: case _C_UINT:
	alignment = __alignof__ (int);
	size = sizeof (int);
	break;
      case _C_FLT:
	alignment = __alignof__ (float);
	size = sizeof (float);
	break;
      case _C_DBL:
	alignment = __alignof__ (double);
	size = sizeof (double);
	break;
      default:
	abort ();
      }
    il->ivar_offset = align (newClass->instanceSize, alignment);
    il->ivar_type = varType;
    il->ivar_name = varName;
    newClass->instanceSize = il->ivar_offset + size;
    ivars->ivar_count++; 
  }
  return newClass;
}

- (void)at: (SEL)aSel addMethod: (IMP)aMethod
{
  if (!dtable)
    {
      if (!superclass)
        raiseEvent (InvalidCombination,
                    "must specify superclass before adding methods to a created class\n");
      
      dtable = sarray_lazy_copy (superclass->dtable);
    }
  
  sarray_at_put_safe (dtable, (size_t) aSel->sel_id, aMethod);
}

- createEnd
{
  if (!dtable)
    dtable = sarray_lazy_copy (superclass->dtable);
  
  setClass (self, metaobjects);
  metaobjects = nil;
  
  setBit (info, _CLS_DEFINEDCLASS, 1);

  return self;
}
@end

@implementation BehaviorPhase_s

PHASE(CreatingOnly)

- (void)setNextPhase: aBehaviorPhase
{
  nextPhase = aBehaviorPhase;
}

- updateArchiver
{
  lispArchiverPut ([((BehaviorPhase_s *) self)->definingClass name], self);
  return self;
}

- lispOut: stream
{
  struct objc_ivar_list *ivars = ((Class_s *) self)->ivarList;
  unsigned i, count = ivars->ivar_count;

  [stream catC: "(make-class 'Class "];

  for (i = 0; i < count; i++)
    {
      [stream catC: " #:"];
      [stream catC: ivars->ivar_list[i].ivar_name];
      [stream catC: " '"];
      switch (*ivars->ivar_list[i].ivar_type)
	{
	case _C_INT:
	  [stream catC: "int"];
	  break;
	case _C_UINT:
	  [stream catC: "unsigned"];
	  break;
	case _C_FLT:
	  [stream catC: "float"];
	  break;
	case _C_DBL:
	  [stream catC: "double"];
	  break;
	}
      [stream catC: "\n"];
    }
  [stream catC: ")\n"];
  return self;
}

@end
