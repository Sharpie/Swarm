// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
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
#import "internal.h" // fcall_type_{size,alignment}, alignsizeto

#include <misc.h> // strncmp, isdigit

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
                  mdefs = _obj_initAlloc (sizeof *mdefs);
                  mdefs->next = (methodDefs_t)classData->metaobjects;
                  classData->metaobjects = (id)mdefs;
                  mdefs->interfaceID = interfaceID;
                  mdefs->firstEntry = mnext + 1;
                  mdefs->count = count;
                }
              if (mnext < methods->method_list)
                break;
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

- setClass: aClass
{
  metaobjects = aClass;
  // later -- reallocate self if class defines additional class vars
  return self;
}

- setSuperclass: aClass
{
  superclass = aClass;
  return self;
}

- setDefiningClass: aClass
{
  definingClass = aClass;
  info = ((Class_s *) aClass)->info;
  instanceSize = ((Class_s *) aClass)->instanceSize;
  ivarList = ((Class_s *) aClass)->ivarList;
  methodList = ((Class_s *) aClass)->methodList;
  return self;
}

- at: (SEL)aSel addMethod: (IMP)aMethod
{
  if (!dtable)
    {
      if (!superclass)
        raiseEvent (InvalidCombination,
                    "must specify superclass before adding methods to a created class\n");
      
      dtable = sarray_lazy_copy (superclass->dtable);
    }
  
  sarray_at_put_safe (dtable, (size_t) aSel->sel_id, aMethod);
  return self;
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

- lispInCreate: expr
{
  id aZone = [expr getZone];
  id <Index> li = [expr begin: aZone];
  id key, val;

  Class newClass = copyClass ((Class) self);
  
  while ((key = [li next]) != nil)
    {
      const char *varName;

      if (!keywordp (key))
        raiseEvent (InvalidArgument, "expecting keyword [%s]", [key name]);

      if ((val = [li next]) == nil)
        raiseEvent (InvalidArgument, "missing value");
      
      varName = ZSTRDUP (aZone, [key getKeywordName]);
      
      if (stringp (val))
        addVariable (newClass, varName,
                     fcall_type_for_lisp_type ([val getC]), 0, NULL);
      else if (archiver_list_p (val))
        {
          id index = [val begin: aZone];
          id first = [index next];
          unsigned rank = [val getCount] - 2;
          fcall_type_t baseType;
          
          if (!stringp (first))
            raiseEvent (InvalidArgument, "argument should be a string");
          
          if (strcmp ([first getC], "array") != 0)
            raiseEvent (InvalidArgument, "argument should be \"array\"");

          {
            id second = [index next];

            if (!stringp (second))
              raiseEvent (InvalidArgument, "array type should be a string");
            
            baseType = fcall_type_for_lisp_type ([second getC]);
          }
          
          {
            id dimCountValue;
            unsigned dims[rank];
            unsigned i;
            
            for (i = 0; (dimCountValue = [index next]); i++)
              {
                if (!valuep (dimCountValue))
                  raiseEvent (InvalidArgument,
                              "array dimension count should be a value");
                dims[i] = [dimCountValue getUnsigned];
              }
            addVariable (newClass, varName, baseType, rank, dims);
          }
          [index drop];
        }
      else
        raiseEvent (InvalidArgument, "argument should be string or list");
    }
  [li drop];
  return newClass;
}


- hdf5InCreate: hdf5Obj
{
  [hdf5Obj setBaseTypeObject: self];
  return [hdf5Obj getClass];
}

- lispOutShallow: stream
{
  struct objc_ivar_list *ivars = ((Class_s *) self)->ivarList;
  unsigned i, count = ivars->ivar_count;

  [stream catStartMakeClass: [self name]];
  for (i = 0; i < count; i++)
    {
      [stream catSeparator];
      [stream catKeyword: ivars->ivar_list[i].ivar_name];
      [stream catSeparator];
      [stream catType: ivars->ivar_list[i].ivar_type];
    }
  [stream catEndMakeClass];
  return self;
}

- hdf5OutShallow: hdf5Obj
{
  raiseEvent (NotImplemented, "DefClass / hdf5OutShallow:");
  return nil;
}

- updateArchiver: archiver
{
  [archiver putShallow: [self name] object: self];
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
