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
#import <collections/predicates.h> // keywordp

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
extend_ivar_list (struct objc_ivar_list *ivars, unsigned additional)
{
  unsigned existing = ivars ? ivars->ivar_count : 0;
  unsigned count = existing + additional;
  struct objc_ivar_list *newivars;
  size_t size = sizeof (struct objc_ivar_list) + 
    (count - 1) * sizeof (struct objc_ivar);
  
  if (additional == 0)
    newivars = xmalloc (size);
  else
    newivars = xrealloc (ivars, size);

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

void
addVariable (Class class, const char *varName, const char *varType)
{
  struct objc_ivar *il;
  size_t alignment, size;
  
  class->ivars = extend_ivar_list (class->ivars, 1);
  il = &class->ivars->ivar_list[class->ivars->ivar_count];
  
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
  il->ivar_offset = align (class->instance_size, alignment);
  il->ivar_type = varType;
  il->ivar_name = varName;
  class->instance_size = il->ivar_offset + size;
  class->ivars->ivar_count++; 
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

Class 
copyClass (Class class)
{
  size_t classSize = sizeof (struct objc_class);
  Class newClass = xmalloc (classSize);
  
  memcpy (newClass, class, classSize);
  newClass->ivars = extend_ivar_list (newClass->ivars, 0);
  return newClass;
}

- lispInCreate: expr
{
  id <Index> li = [expr begin: [expr getZone]];
  id key, val;

  Class newClass = copyClass ((Class) self);

  while ((key = [li next]) != nil)
    {
      if (!keywordp (key))
        raiseEvent (InvalidArgument, "expecting keyword [%s]", [key name]);

      if ((val = [li next]) == nil)
        raiseEvent (InvalidArgument, "missing value");
      
      if (!stringp (val))
        raiseEvent (InvalidArgument, "argument should be string");
      
      {
        const char *typeString = [val getC];
        const char *ivarname = strdup ([key getKeywordName]);
        
        if (strcmp (typeString, "int") == 0)
          addVariable (newClass, ivarname, @encode (int));
        else if (strcmp (typeString, "double") == 0)
          addVariable (newClass, ivarname, @encode (double));
        else if (strcmp (typeString, "float") == 0)
          addVariable (newClass, ivarname, @encode (float));
        else
          abort ();
      }
    }
  [li drop];
  return newClass;
}


- hdf5InCreate: expr
{
  raiseEvent (NotImplemented, "DefClass / hdf5InCreate:");
  return nil;
}

- lispIn: expr
{
  return self;
}

- lispOut: stream
{
  struct objc_ivar_list *ivars = ((Class_s *) self)->ivarList;
  unsigned i, count = ivars->ivar_count;

  [stream catC: "(" MAKE_CLASS_FUNCTION_NAME " '"];
  [stream catC: [self name]];
  [stream catC: " "];

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
    }
  [stream catC: ")"];
  return self;
}

- hdf5In: expr
{
  raiseEvent (NotImplemented, "DefClass / hdf5In:");
  return nil;
}

- hdf5Out: stream
{
  raiseEvent (NotImplemented, "DefClass / hdf5Out:");
  return nil;
}

- updateArchiver
{
  lispArchiverPut ([self name], self);
  return self;
}

@end

@implementation BehaviorPhase_s

PHASE(CreatingOnly)

- (void)setNextPhase: aBehaviorPhase
{
  nextPhase = aBehaviorPhase;
}

@end
