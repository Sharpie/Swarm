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
#import <defobj/internal.h> // {alignment,size}_for_objc_type, align

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

static struct objc_ivar_list *
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

void
addVariable (Class class, const char *varName, const char *varType)
{
  struct objc_ivar *il;
  
  class->ivars = extend_ivar_list (class->ivars, 1);
  il = &class->ivars->ivar_list[class->ivars->ivar_count];
  
  il->ivar_offset = alignsizeto (class->instance_size,
                                 alignment_for_objc_type (varType));
  il->ivar_type = SSTRDUP (varType);
  il->ivar_name = SSTRDUP (varName);
  class->instance_size = il->ivar_offset + size_for_objc_type (varType);
  class->ivars->ivar_count++; 
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

Class 
copyClass (Class class)
{
  size_t classSize = sizeof (struct objc_class);
  Class newClass = xmalloc (classSize);
  
  memcpy (newClass, class, classSize);
  newClass->ivars = extend_ivar_list (newClass->ivars, 0);
  return newClass;
}

id
createType (id aZone, const char *typeName)
{
  Class newClass = [CreateDrop class];
  id typeObject = [id_BehaviorPhase_s createBegin: aZone];

  [typeObject setName: ZSTRDUP (aZone, typeName)];
  [typeObject setClass: getClass (newClass)];
  [typeObject setDefiningClass: newClass]; 
  [typeObject setSuperclass: newClass];

  return typeObject;
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
        addVariable (newClass, varName, objc_type_for_lisp_type ([val getC]));
      else if (archiver_list_p (val))
        {
          id index = [val begin: aZone];
          id first = [index next];
          unsigned rank = [val getCount] - 2;
          char typebuf[rank * (DSIZE (unsigned) + 2) + 1 + 1];
          char *p = typebuf;
          const char *baseType;
          
          if (!stringp (first))
            raiseEvent (InvalidArgument, "argument should be a string");
          
          if (strcmp ([first getC], "array") != 0)
            raiseEvent (InvalidArgument, "argument should be \"array\"");

          {
            id second = [index next];

            if (!stringp (second))
              raiseEvent (InvalidArgument, "array type should be a string");
            
            baseType = objc_type_for_lisp_type ([second getC]);
          }
          
          {
            id dimCountValue;
            unsigned i;
            
            while ((dimCountValue = [index next]))
              {
                char numbuf[DSIZE (unsigned) + 1];

                if (!valuep (dimCountValue))
                  raiseEvent (InvalidArgument,
                              "array dimension count should be a value");
                sprintf (numbuf, "%u", [dimCountValue getUnsigned]);
                p = stpcpy (p, "[");
                p = stpcpy (p, numbuf);
              }
            p = stpcpy (p, baseType);
            for (i = 0; i < rank; i++)
              p = stpcpy (p, "]");
            addVariable (newClass, varName, ZSTRDUP (aZone, typebuf));
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
  [stream catEndExpr];
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
