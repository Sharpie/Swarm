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
#import <defobj/internal.h> // {alignment,size}_for_objc_type, align

#include <misc.h> // strncmp, isdigit

#define TYPE_SHORT "short"
#define TYPE_UNSIGNED_SHORT "unsigned short"
#define TYPE_INT "int"
#define TYPE_UNSIGNED "unsigned"
#define TYPE_LONG "long"
#define TYPE_UNSIGNED_LONG "unsigned long"
#define TYPE_FLOAT "float"
#define TYPE_DOUBLE "double"
#define TYPE_STRING "string"
#define TYPE_OBJECT "object"

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

void
addVariable (Class class, const char *varName, const char *varType)
{
  struct objc_ivar *il;
  
  class->ivars = extend_ivar_list (class->ivars, 1);
  il = &class->ivars->ivar_list[class->ivars->ivar_count];
  
  il->ivar_offset = alignto (class->instance_size,
                             alignment_for_objc_type (varType));
  il->ivar_type = varType;
  il->ivar_name = varName;
  class->instance_size = il->ivar_offset + size_for_objc_type (varType);
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

static const char *
objc_type_for_lisp_type (const char *lispTypeString)
{
  if (strcmp (lispTypeString, TYPE_SHORT) == 0)
    return @encode (short);
  else if (strcmp (lispTypeString, TYPE_UNSIGNED_SHORT) == 0)
    return @encode (unsigned short);
  else if (strcmp (lispTypeString, TYPE_INT) == 0)
    return @encode (int);
  else if (strcmp (lispTypeString, TYPE_UNSIGNED) == 0)
    return @encode (unsigned);
  else if (strcmp (lispTypeString, TYPE_LONG) == 0)
    return @encode (long);
  else if (strcmp (lispTypeString, TYPE_UNSIGNED_LONG) == 0)
    return @encode (unsigned long);
  else if (strcmp (lispTypeString, TYPE_FLOAT) == 0)
    return @encode (float);
  else if (strcmp (lispTypeString, TYPE_DOUBLE) == 0)
    return @encode (double);
  else if (strcmp (lispTypeString, TYPE_STRING) == 0)
    return @encode (const char *);
  else if (strcmp (lispTypeString, TYPE_OBJECT) == 0)
    return @encode (id);
  else
    abort ();
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
      
      varName = strdup ([key getKeywordName]);
      
      if (stringp (val))
        addVariable (newClass, varName, objc_type_for_lisp_type ([val getC]));
      else if (listp (val))
        {
          id index = [val begin: aZone];
          id first = [index next];
          unsigned rank = [val getCount] - 2;
          char typebuf[rank * (sizeof (unsigned) * 8 + 2) + 1 + 1];
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
                char numbuf[sizeof (unsigned) * 8 + 1];
                if (!valuep (dimCountValue))
                  raiseEvent (InvalidArgument,
                              "array dimension count should be a value");
                sprintf (numbuf, "%u", [dimCountValue getInteger]);
                p = stpcpy (p, "[");
                p = stpcpy (p, numbuf);
              }
            p = stpcpy (p, baseType);
            for (i = 0; i < rank; i++)
              p = stpcpy (p, "]");
            addVariable (newClass, varName, strdup (typebuf));
          }
          [index drop];
        }
      else
        raiseEvent (InvalidArgument, "argument should be string or list");
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

static const char *
process_type (const char *varType,
             void (*func) (unsigned dim, unsigned count))
{
  const char *baseType;
  unsigned dimnum;

  void expand_type (const char *type)
    {
      switch (*type)
        {
        case _C_SHT:
          baseType = TYPE_SHORT;
          break;
        case _C_USHT:
          baseType = TYPE_UNSIGNED_SHORT;
          break;
        case _C_INT:
          baseType = TYPE_INT;
          break;
        case _C_UINT:
          baseType = TYPE_UNSIGNED;
          break;
        case _C_LNG:
          baseType = TYPE_LONG;
          break;
        case _C_ULNG:
          baseType = TYPE_UNSIGNED_LONG;
          break;
        case _C_FLT:
          baseType = TYPE_FLOAT;
          break;
        case _C_DBL:
          baseType = TYPE_DOUBLE;
          break;
        case _C_CHARPTR:
          baseType = TYPE_STRING;
          break;
        case _C_ID:
          baseType = TYPE_OBJECT;
          break;
        case _C_ARY_B:
          type++;
          {
            char *tail;
            unsigned count = strtoul (type, &tail, 10);
            
            if (func)
              func (dimnum, count);
            dimnum++;
            expand_type (tail);
          }
          break;
        default:
          abort ();
        }
    }
  dimnum = 0;
  expand_type (varType);
  return baseType;
}

- lispOut: stream deep: (BOOL)deepFlag
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
      [stream catC: " "];
      {
        const char *type = ivars->ivar_list[i].ivar_type;
        
        if (*type == _C_ARY_B)
          {
            [stream catC: "(array '"];
            [stream catC: process_type (type, NULL)];
            {
              void outputCount (unsigned dim, unsigned count)
                {
                  char buf[sizeof (count) * 8 + 2];
                  
                  sprintf (buf, " %u", count);
                  [stream catC: buf];
                }
              process_type (type, outputCount);
            }
            [stream catC: ")"];
          }
        else
          {
            [stream catC: "'"];
            [stream catC: process_type (type, NULL)];
          }
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

- hdf5Out: stream deep: (BOOL)deepFlag
{
  raiseEvent (NotImplemented, "DefClass / hdf5Out:deep:");
  return nil;
}

- updateArchiver
{
  lispArchiverPut ([self name], self, NO);
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
