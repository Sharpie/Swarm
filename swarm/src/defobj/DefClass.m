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
Name:         DefClass.m
Description:  class with variables and/or methods defined at runtime 
Library:      defobj
*/

#import <defobj/DefClass.h>
#import <defobj/Program.h>
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
_obj_getClassData (Class class)
{
  classData_t classData;

#if SWARM_OBJC_DONE
  classData = (classData_t)_obj_classes[CLS_GETNUMBER (class) - 1];
  if (!classData)
    {
      classData = _obj_initAlloc (sizeof *classData);
      _obj_classes[CLS_GETNUMBER (class) - 1] = (id) classData;
    }
#else
  //printf("_obj_getClassData lookup: %p %s\n", class, swarm_class_getName (class));
  if (!swarm_hash_is_key_in_hash (_obj_buckets, class)) {
    //printf("classData not found in hash.\n");
    classData = _obj_initAlloc (sizeof *classData);
    classData->classID = class;
    swarm_hash_add (&_obj_buckets, class, classData);
  }
  classData = (classData_t) swarm_hash_value_for_key (_obj_buckets, class);
#endif
  return classData;
}

//
// _obj_initMethodInterfaces() -- function to initialize methods by interface
//
methodDefs_t
_obj_initMethodInterfaces (Class class)
{
#if SWARM_OBJC_DONE
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
#else
  unsigned int outCount;
  int i, count;
  id interfaceID;
  const char *mname;
  methodDefs_t mdefs = NULL;

  ObjcMethod *methodList = swarm_class_copyMethodList(class, &outCount);
  //printf("%d methods\n", outCount);
  count = 0;
  interfaceID = Using;
  for (i = outCount - 1; i >= -1; --i) {
    //if (i != -1) printf("%s\n", swarm_sel_getName(swarm_method_getName(methodList[i])));
    if ((i == -1)
	|| (strncmp ((mname = swarm_sel_getName(swarm_method_getName(methodList[i]))),
		     "_I_", 3) == 0)
	|| (strncmp ((mname = swarm_sel_getName(swarm_method_getName(methodList[i]))),
		     "_C_", 3) == 0)) {
      if (count) {
	methodDefs_t holdmdefs = mdefs;
	mdefs = _obj_initAlloc (sizeof *mdefs);
	//mdefs->next = (methodDefs_t)classData->metaobjects;
	mdefs->next = holdmdefs;
	//classData->metaobjects = (id)mdefs;
	mdefs->interfaceID = interfaceID;
	mdefs->firstEntry = &(methodList[i + 1]);
	mdefs->count = count;
      }
      if (i == -1) continue;
      interfaceID = swarm_method_getImplementation(methodList[i]) (nil, (SEL)0);
      count = 0;
    } else
      count++;
  }

  return mdefs;
#endif
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

- setClass: (Class)aClass
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

- setDefiningClass: (Class)aClass
{
  definingClass = aClass;
  //info = aClass->info;
  //instanceSize = aClass->instance_size;
  //ivarList = aClass->ivars;
  //methodList = aClass->methods;
  return self;
}

- at: (SEL)aSel addMethod: (IMP)aMethod
{
#if SWARM_OBJC_DONE
  if (!dtable)
    {
      if (!superclass)
        raiseEvent (InvalidCombination,
                    "must specify superclass before adding methods to a created class\n");
      
      dtable = sarray_lazy_copy (superclass->dtable);
    }
  
  sarray_at_put_safe (dtable, (size_t) aSel->sel_id, aMethod);
#else
  // SWARM_OBJC_TODO - do we need meta information about methods?
#endif
  return self;
}

- createEnd
{
#if SWARM_OBJC_DONE
  if (!dtable)
    dtable = sarray_lazy_copy (superclass->dtable);
  
  setClass (self, metaobjects);
  metaobjects = nil;
  
  setBit (info, _CLS_DEFINEDCLASS, 1);
#else
  swarm_class_setDefinedClassBit (self->definingClass, YES);
  metaobjects = nil;
#endif
  return self;
}

- lispInCreate: expr
{
  id aZone = [expr getZone];
  id <Index> li = [expr begin: aZone];
  id key, val;

#if SWARM_OBJC_DONE
  Class newClass = class_copy ((Class) self);
#else
  Class newClass = swarm_objc_allocateClassPairCopy((Class) self, class_generate_name(), 0);
#endif
  
  while ((key = [li next]) != nil)
    {
      const char *varName;

      if (!keywordp (key))
        raiseEvent (InvalidArgument, "expecting keyword [%s]", [key name]);

      if ((val = [li next]) == nil)
        raiseEvent (InvalidArgument, "missing value");
      
      varName = ZSTRDUP (aZone, [key getKeywordName]);
      
      if (stringp (val))
        class_addVariable (newClass, varName,
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
            class_addVariable (newClass, varName, baseType, rank, dims);
          }
          [index drop];
        }
      else
        raiseEvent (InvalidArgument, "argument should be string or list");
    }
  [li drop];

  swarm_objc_registerClassPair(newClass);

  return newClass;
}


- hdf5InCreate: hdf5Obj
{
  [hdf5Obj setBaseTypeObject: self];
  return [hdf5Obj getClass];
}

- (void)lispOutShallow: stream
{
#if SWARM_OBJC_DONE
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
#else
  unsigned i, outCount;
  ObjcIvar *ivars = swarm_class_copyIvarList(swarm_object_getClass(self), &outCount);

  [stream catStartMakeClass: [self name]];
  for (i = 0; i < outCount; i++)
    {
      [stream catSeparator];
      [stream catKeyword: swarm_ivar_getName(ivars[i])];
      [stream catSeparator];
      [stream catType: swarm_ivar_getTypeEncoding(ivars[i])];
    }
  [stream catEndMakeClass];
  if (ivars) free(ivars);
#endif
}

- (void)hdf5OutShallow: hdf5Obj
{
  raiseEvent (NotImplemented, "DefClass / hdf5OutShallow:");
}

- (void)updateArchiver: archiver
{
  [archiver putShallow: [self name] object: self];
}

@end

@implementation BehaviorPhase_s

PHASE(CreatingOnly)

- (void)setNextPhase: aBehaviorPhase
{
  nextPhase = aBehaviorPhase;
}

@end
